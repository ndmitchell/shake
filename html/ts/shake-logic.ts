/* tslint:disable */
"use strict";

//////////////////////////////////////////////////////////////////////
// SUMMARY

class Summary {
    sumExecution: seconds = 0 // build time in total
    countTraceLast : int = 0 // traced commands run
    maxTraceStopLast: seconds = 0 // time the last traced command stopped
}

function summary(dat: Profile[]): Summary
{
    const res = new Summary();

    // Fold over dat to produce the summary
    for (const e of dat) {
        var isLast = e.built === 0;
        res.sumExecution += e.execution;
        var traces = e.traces;
        if (!traces) continue;
        for (const t of traces) {
            var time = t.stop - t.start;
            res.countTraceLast += isLast ? 1 : 0;
            res.maxTraceStopLast = Math.max(res.maxTraceStopLast, isLast ? t.stop : 0);
        }
    }
    return res;
}


/////////////////////////////////////////////////////////////////////
// PREPARATION

interface ProfileEx extends Profile {
    rdeps: int[]; // the 1-level reverse dependencies, index into Entry
    cost: seconds; // cost if this item rebuilds
}

class Prepare {
    original: ProfileEx[];
    summary: Summary;
    dependsOnThis: (from: int, match: string | RegExp) => boolean;
    thisDependsOn: (from: int, match: string | RegExp) => boolean;
    dependsOnThisTransitive: (from: int, match: string | RegExp) => boolean;
    thisDependsOnTransitive: (from: int, match: string | RegExp) => boolean;
}

// Mutate the input data, adding in rdeps, being the 1-level reverse dependencies
function addRdeps(dat: Profile[]): (Profile & { rdeps: int[] })[]
{
    // find the reverse dependencies
    var rdeps: MapInt<void>[] = [];
    for (let i = 0; i < dat.length; i++)
        rdeps[i] = {};
    for (let i = 0; i < dat.length; i++) {
        for (const j of dat[i].depends)
            rdeps[j][i] = null;
    }

    var res: (Profile & { rdeps?: int[] })[] = dat;
    for (let i = 0; i < rdeps.length; i++) {
        var ans : number[] = [];
        for (const j in rdeps[i])
            ans.push(Number(j));
        res[i].rdeps = ans;
    }
    return <(Profile & { rdeps: int[] })[]>res;
}


// Given an array of indices, calculate the cost to rebuild if all of them change
// You must call addRdeps and addCost first
function calcRebuildCosts(dat: ProfileEx[], xs : int[]) : seconds
{
    const seen: MapInt<void> = {};
    let tot : seconds = 0;
    function f(i : int) {
        if (i in seen) return;
        seen[i] = null;
        tot += dat[i].execution;
        for (const j of dat[i].rdeps)
            f(j);
    }
    if (xs.length === 1 && dat[xs[0]].depends.length === 1)
        tot = dat[dat[xs[0]].depends[0]].cost + dat[xs[0]].execution;
    else {
        for (const x of xs)
            f(x);
    }
    return tot;
}

// Mutate the dat data, adding in cost, being the cost to rebuild if this item changes
function addCost(dat: (Profile & { rdeps: int[] })[]): ProfileEx[]
{
    const res: (Profile & { rdeps: int[], cost?: seconds })[] = dat;
    for(let i = 0; i < dat.length; i++){
        // This call is type safe because calcRebuildCosts only ever looks at earlier items,
        // and those earlier items all have their cost filled in
        res[i].cost = calcRebuildCosts(<ProfileEx[]>res, [i]);
    }
    return <ProfileEx[]>res;
}

function prepare(dat_: Profile[]): Prepare
{
    const sum = summary(dat_);
    const dat = addCost(addRdeps(dat_));

    function toHash(r: RegExp | string): string {
        return typeof r === "string" ? "$" + r : "/" + r.source;
    }

    function findDirect(key: string): (from: int, match: string | RegExp) => boolean {
        const c = cache(toHash, function (r) {
            const want: MapInt<void> = {};
            for (const e of dat) {
                if (testRegExp(r, e.name)) {
                    const deps : int[] = (<any>(e))[key];
                    for (const j of deps)
                        want[j] = null;
                }
            }
            return want;
        });
        return (i, r) => i in c(r);
    }

    function findTransitive(key: string, dirFwd: boolean): (from: int, match: string | RegExp) => boolean {
        var c = cache(toHash, function (r) {
            var want: MapInt<void> = {};
            for (var i = 0; i < dat.length; i++) {
                var j = dirFwd ? i : dat.length - 1 - i;
                if ((j in want) || testRegExp(r, dat[j].name)) {
                    want[j] = null;
                    const deps: int[] = (<any>(dat[j]))[key];
                    for (const k of deps)
                        want[k] = null;
                }
            }
            return want;
        });
        return (i, r) => i in c(r);
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


/////////////////////////////////////////////////////////////////////
// RULES

function colorAnd(c1: color, c2: color) {
    return c1 === null ? c2 : c1 === c2 ? c1 : undefined;
}

class Result {
    items: int[];
    text: color;
    back: color;
}

class ResultTable {
    name: string;
    count: int;
    time: seconds;
    back: color;
    text: color;
    cost: seconds;
    leaf: string | boolean; // true, false, "" (none), "both" (both)
    run: timestamp;
    unchanged: string | boolean;
}

function ruleTable(dat: Prepare, query: string): ResultTable[]
{
    function bools(x : string | boolean, y : boolean) : string | boolean {
        return x === "" ? y : x === y ? x : "both";
    }

    var res = ruleFilter(dat, query);
    var ans : ResultTable[] = [];
    for (var s in res) {
        var xs = res[s].items;
        var time = 0;
        var leaf: string | boolean = "";
        var unchanged: string | boolean = "";
        var run = 100000;
        for (var i = 0; i < xs.length; i++) {
            var x = dat.original[xs[i]];
            time += x.execution;
            leaf = bools(leaf, x.depends.length === 0);
            unchanged = bools(unchanged, x.changed !== x.built);
            run = Math.min(run, x.built);
        }
        ans.push({ name: s, count: xs.length, time: time, back: res[s].back, text: res[s].text, cost: calcRebuildCosts(dat.original, xs), leaf: leaf, run: run, unchanged: unchanged });
    }
    return ans;
}

class ResultGraph
{
    name: string;
    text: color;
    back: color;
    parents: int[];
    ancestors: int[];
}

function ruleGraph(dat : Prepare, query : string) : ResultGraph[]
{
    var res = ruleFilter(dat, query);

    var map : MapInt<int[]> = {}; // which nodes a node lives at

    // loop through each value in res, putting it into map (these are parents)
    // for any not present, descend through the dat.original list, if you aren't included, add, if you are included, skip
    var direct: MapInt<int> = {};
    var ind = -1;
    for (var s in res) {
        ind++;
        var xs = res[s].items;
        for (var i = 0; i < xs.length; i++)
            direct[xs[i]] = ind;
    }
    function getDirect(key : int) : int[] {
        return key in direct ? [direct[key]] : [];
    }

    var indirect: MapInt<int[]> = {};
    function getIndirect(key : int) : int[] {
        if (key in indirect) return indirect[key];
        if (key in direct) return [];
        var ds = dat.original[key].depends;
        const res: int[][] = [];
        for (var j = 0; j < ds.length; j++) {
            res.push(getIndirect(ds[j]));
            res.push(getDirect(ds[j]));
        }
        const res2: int[] = concatNub(res);
        indirect[key] = res2;
        return res2;
    }

    var ans : ResultGraph[] = [];
    for (var s in res) {
        var xs = res[s].items;
        var ds : int[][] = [];
        var is : int[][] = [];
        for (var i = 0; i < xs.length; i++) {
            var depends = dat.original[xs[i]].depends;
            for (var j = 0; j < depends.length; j++) {
                ds.push(getDirect(depends[j]));
                is.push(getIndirect(depends[j]));
            }
        }
        ans.push({ name: s, text: res[s].text, back: res[s].back, parents: concatNub(ds), ancestors: concatNub(is) });
    }
    return ans;
}


/////////////////////////////////////////////////////////////////////
// COMMANDS

function commandFilter(last: boolean, dat: Prepare, query: string): MapString<{ items: Trace[], text: color, back: color }>
{
    queryData = dat;
    var f = readQuery(query);
    var res: MapString<{ items: Trace[], text: color, back: color }> = {};

    for (queryKey = 0; queryKey < dat.original.length; queryKey++) {
        queryVal = dat.original[queryKey];
        if (last && queryVal.built !== 0) continue;

        var val = recordCopy(queryVal);
        var ts = queryVal.traces || [];
        queryVal = val;
        queryName = queryVal.name;
        queryBackColor = null;
        queryTextColor = null;
        for (var i = 0; i < ts.length; i++) {
            queryVal.traces = [ts[i]];
            queryGroup = null;
            if (f()) {
                if (queryGroup === null) queryGroup = ts[i].command;
                if (!(queryGroup in res))
                    res[queryGroup] = { items: [ts[i]], text: queryTextColor, back: queryBackColor };
                else {
                    var c = res[queryGroup];
                    c.items.push(ts[i]);
                    c.text = colorAnd(c.text, queryTextColor);
                    c.back = colorAnd(c.back, queryBackColor);
                }
            }
        }
    }
    return res;
}

class CommandTable {
    name: string;
    count: int;
    text: color;
    back: color;
    time: seconds;
}

function commandTable(dat: Prepare, query : string) : CommandTable[]
{
    var res = commandFilter(false, dat, query);
    var ans : CommandTable[] = [];
    for (var s in res) {
        var xs = res[s].items;
        var time = 0;
        for (const t of xs)
            time += t.stop - t.start;
        ans.push({ name: s, count: xs.length, text: res[s].text, back: res[s].back, time: time });
    }
    return ans;
}

function commandPlot(dat: Prepare, query: string, buckets: int): MapString<{ items: number[], back: color }>
{
    var end = dat.summary.maxTraceStopLast;
    var res = commandFilter(true, dat, query);
    var ans: MapString<{ items: number[], back: color }> = {};
    for (var s in res) {
        var ts = res[s].items;
        var xs : number[] = [];
        for (var i = 0; i <= buckets; i++)
            xs.push(0); // fill with 1 more element, but the last bucket will always be 0

        for (const t of ts) {
            var start = t.start * buckets / end;
            var stop = t.stop * buckets / end;

            if (Math.floor(start) === Math.floor(stop))
                xs[Math.floor(start)] += stop - start;
            else {
                for (var j = Math.ceil(start); j < Math.floor(stop); j++)
                    xs[j]++;
                xs[Math.floor(start)] += Math.ceil(start) - start;
                xs[Math.floor(stop)] += stop - Math.floor(stop);
            }
        }
        ans[s] = { items: xs.slice(0, buckets), back: res[s].back || null };
    }
    return ans;
}
