/*jsl:option explicit*/
/*jsl:import shake-util.js*/
"use strict";

//////////////////////////////////////////////////////////////////////
// TYPES

type timestamp = int

type Trace =
    {
        command: string,
        start: seconds,
        stop: seconds
    }

type Entry =
    {
        name: string, // Name of the thing I built
        built: timestamp, // Timestamp at which I was built
        changed: timestamp, // Timestamp at which I last changed
        depends: int[], // Which 0-based indexes I depended on (always lower than my index)
        execution: seconds, // Seconds I took to execute
        traces?: Trace[] // List of traces
    }

//////////////////////////////////////////////////////////////////////
// SUMMARY

class Summary {
    count: int = 0; // number of rules run
    countLast: int = 0 // number of rules run in the last run
    highestRun: timestamp = 0 // highest run you have seen (add 1 to get the count of runs)
    sumExecution: seconds = 0 // build time in total
    maxExecution: seconds = 0 // longest build rule
    maxExecutionName: string = "" // longest build rule
    countTrace: int = 0; countTraceLast : int = 0 // traced commands run
    sumTrace: seconds = 0; sumTraceLast: seconds = 0 // time running traced commands
    maxTrace: seconds = 0 // longest traced command
    maxTraceName: string = "" // longest trace command
    maxTraceStopLast: seconds = 0 // time the last traced command stopped
}

function /* export */ summary(dat : Entry[]) : Summary
{
    var res = new Summary();

    // Fold over dat to produce the summary
    res.count = dat.length;
    for (var i = 0; i < dat.length; i++) {
        var isLast = dat[i].built === 0;
        res.countLast += isLast ? 1 : 0;
        res.sumExecution += dat[i].execution;
        res.maxExecution = Math.max(res.maxExecution, dat[i].execution);
        if (res.maxExecution === dat[i].execution) res.maxExecutionName = dat[i].name;
        res.highestRun = Math.max(res.highestRun, dat[i].changed); // changed is always greater or equal to built
        var traces = dat[i].traces;
        if (!traces) continue;
        for (var j = 0; j < traces.length; j++) {
            var time = traces[j].stop - traces[j].start;
            res.countTrace += 1;
            res.countTraceLast += isLast ? 1 : 0;
            res.sumTrace += time;
            res.sumTraceLast += isLast ? time : 0;
            res.maxTrace = Math.max(res.maxTrace, time);
            if (res.maxTrace == time) res.maxTraceName = traces[j].command;
            res.maxTraceStopLast = Math.max(res.maxTraceStopLast, isLast ? traces[j].stop : 0);
        }
    }
    return res;
}

function /* export */ showSummary(sum : Summary) : string[]
{
    return ["This database has tracked " + (sum.highestRun + 1) + " run" + plural(sum.highestRun + 1) + "."
        , "There are " + sum.count + " rules (" + sum.countLast + " rebuilt in the last run)."
        , "Building required " + sum.countTrace + " traced commands (" + sum.countTraceLast + " in the last run)."
        , "The total (unparallelised) build time is " + showTime(sum.sumExecution) + " of which " + showTime(sum.sumTrace) + " is traced commands."
        , "The longest rule takes " + showTime(sum.maxExecution) + " (" + sum.maxExecutionName + ") and the longest traced command takes " + showTime(sum.maxTrace) + " (" + sum.maxTraceName + ")."
        , "Last run gave an average parallelism of " + (sum.maxTraceStopLast === 0 ? 0 : sum.sumTraceLast / sum.maxTraceStopLast).toFixed(2) + " times over " + showTime(sum.maxTraceStopLast) + "."
    ];
}


/////////////////////////////////////////////////////////////////////
// PREPARATION

type EntryEx = Entry &
    {
        rdeps: int[], // the 1-level reverse dependencies, index into Entry
        cost: seconds // cost if this item rebuilds
    }

type Prepare =
    {
        original: EntryEx[],
        summary: Summary,
        dependsOnThis: any, // FIXME: Refine
        thisDependsOn: any, // FIXME: Refine
        dependsOnThisTransitive: any, // FIXME: Refine
        thisDependsOnTransitive: any // FIXME: Refine
    }

// Mutate the input data, adding in rdeps, being the 1-level reverse dependencies
function addRdeps(dat: Entry[]): (Entry & { rdeps: int[] })[]
{
    // find the reverse dependencies
    var rdeps: {}[] = [];
    for (var i = 0; i < dat.length; i++)
        rdeps[i] = {};
    for (var i = 0; i < dat.length; i++) {
        var deps = dat[i].depends;
        for (var j = 0, n = deps.length; j < n; j++)
            rdeps[deps[j]][i] = true;
    }

    var res: (Entry & { rdeps?: int[] })[] = dat;
    for (var i = 0; i < rdeps.length; i++) {
        var ans = [];
        for (var jj in rdeps[i])
            ans.push(Number(jj));
        res[i].rdeps = ans;
    }
    return <(Entry & { rdeps: int[] })[]>res;
}


// Given an array of indices, calculate the cost to rebuild if all of them change
// You must call addRdeps and addCost first
function calcRebuildCosts(dat: EntryEx[], xs : int[]) : seconds
{
    var seen = {};
    var tot : seconds = 0;
    function f(i : int) {
        if (seen[i]) return;
        seen[i] = true;
        tot += dat[i].execution;
        var deps = dat[i].rdeps;
        for (var j = 0, n = deps.length; j < n; j++)
            f(deps[j]);
    }
    if (xs.length === 1 && dat[xs[0]].depends.length === 1)
        tot = dat[dat[xs[0]].depends[0]].cost + dat[xs[0]].execution;
    else {
        for (var i = 0, n = xs.length; i < n; i++)
            f(xs[i]);
    }
    return tot;
}

// Mutate the dat data, adding in cost, being the cost to rebuild if this item changes
function addCost(dat: (Entry & { rdeps: int[] })[]): EntryEx[]
{
    var res: (Entry & { rdeps: int[], cost?: seconds })[] = dat;
    for(var i = 0; i < dat.length; i++){
        // This call is type safe because calcRebuildCosts only ever looks at earlier items,
        // and those earlier items all have their cost filled in
        res[i].cost = calcRebuildCosts(<EntryEx[]>res, [i]);
    }
    return <EntryEx[]>res;
}

function prepare(sum: Summary, dat_: Entry[]): Prepare
{
    var dat = addCost(addRdeps(dat_));

    function toHash(r) { return typeof r === "string" ? "$" + r : "/" + r.source; }

    function findDirect(key) {
        var c = cache(toHash, function (r) {
            var want = {};
            for (var i = 0; i < dat.length; i++) {
                if (testRegExp(r, dat[i].name)) {
                    var deps = dat[i][key];
                    for (var j = 0; j < deps.length; j++)
                        want[deps[j]] = true;
                }
            }
            return want;
        });
        return function (i, r) {
            if (i in c(r))
                return true;
            else
                return false;
        };
    }

    function findTransitive(key, dirFwd) {
        var c = cache(toHash, function (r) {
            var want = {};
            for (var i = 0; i < dat.length; i++) {
                var j = dirFwd ? i : dat.length - 1 - i;
                if ((j in want) || testRegExp(r, dat[j].name)) {
                    want[j] = true;
                    var deps = dat[j][key];
                    for (var k = 0; k < deps.length; k++)
                        want[deps[k]] = true;
                }
            }
            return want;
        });
        return function (i, r) { return i in c(r); };
    }

    return {
        original: dat
        , summary: sum
        , dependsOnThis: findDirect("rdeps")
        , thisDependsOn: findDirect("depends")
        , dependsOnThisTransitive: findTransitive("depends", false)
        , thisDependsOnTransitive: findTransitive("rdeps", true)
    };
}
