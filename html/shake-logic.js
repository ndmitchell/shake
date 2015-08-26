/*jsl:option explicit*/
/*jsl:import shake-util.js*/
"use strict";

// Data
//     {name :: String
//     ,built :: Int
//     ,changed :: Int
//     ,depends :: [Int]
//     ,execution :: Double
//     ,traces :: [Trace]
//     }
//
// Trace
//     {start :: Double
//     ,stop :: Double
//     ,command :: String
//     }


function bools(x,y)
{
    return x === "" ? y : x === y ? x : "both";
}

function colorAnd(c1, c2)
{
    return c1 === null ? c2 : c1 === c2 ? c1 : undefined;
}

//////////////////////////////////////////////////////////////////////
// SUMMARY

function /* export */ summary(dat) // :: Data -> Summary
{
    // Summary statistics
    var res =
        {count : 0 // :: Int, number of rules run
        ,countLast : 0 // :: Int, number of rules run in the last run
        ,highestRun : 0 // :: Int, highest run you have seen (add 1 to get the count of runs)
        ,sumExecution : 0 // :: Seconds, build time in total
        ,maxExecution : 0 // :: Seconds, longest build rule
        ,maxExecutionName : "" // :: String, longest build rule
        ,countTrace : 0, countTraceLast : 0 // :: Int, traced commands run
        ,sumTrace : 0, sumTraceLast : 0 // :: Seconds, time running traced commands
        ,maxTrace : 0 // :: Seconds, longest traced command
        ,maxTraceName : "" // :: String, longest trace command
        ,maxTraceStopLast : 0 // :: Seconds, time the last traced command stopped
        };

    // Fold over dat to produce the summary
    res.count = dat.length;
    for (var i = 0; i < dat.length; i++)
    {
        var isLast = dat[i].built === 0;
        res.countLast += isLast ? 1 : 0;
        res.sumExecution += dat[i].execution;
        res.maxExecution = Math.max(res.maxExecution, dat[i].execution);
        if (res.maxExecution === dat[i].execution) res.maxExecutionName = dat[i].name;
        res.highestRun = Math.max(res.highestRun, dat[i].changed); // changed is always greater or equal to built
        var traces = dat[i].traces;
        if (!traces) continue;
        for (var j = 0; j < traces.length; j++)
        {
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

function /* export */ showSummary(sum) // Summary -> [String]
{
    return ["This database has tracked " + (sum.highestRun+1) + " run" + plural(sum.highestRun+1) + "."
           ,"There are " + sum.count + " rules (" + sum.countLast + " rebuilt in the last run)."
           ,"Building required " + sum.countTrace + " traced commands (" + sum.countTraceLast + " in the last run)."
           ,"The total (unparallelised) build time is " + showTime(sum.sumExecution) + " of which " + showTime(sum.sumTrace) + " is traced commands."
           ,"The longest rule takes " + showTime(sum.maxExecution) + " (" + sum.maxExecutionName + ") and the longest traced command takes " + showTime(sum.maxTrace) + " (" + sum.maxTraceName + ")."
           ,"Last run gave an average parallelism of " + (sum.maxTraceStopLast === 0 ? 0 : sum.sumTraceLast / sum.maxTraceStopLast).toFixed(2) + " times over " + showTime(sum.maxTraceStopLast) + "."
           ];
}


/////////////////////////////////////////////////////////////////////
// PREPARATION

// Mutate the input data, adding in rdeps, being the 1-level reverse dependencies
function addRdeps(dat) // Data -> Mutate Data{+rdeps}
{
    // find the reverse dependencies
    var rdeps = [];
    for (var i = 0; i < dat.length; i++)
        rdeps[i] = {};
    for (var i = 0; i < dat.length; i++)
    {
        var deps = dat[i].depends;
        for (var j = 0, n = deps.length; j < n; j++)
            rdeps[deps[j]][i] = true;
    }
    for (var i = 0; i < rdeps.length; i++)
    {
        var ans = [];
        for (var j in rdeps[i])
            ans.push(Number(j));
        dat[i].rdeps = ans;
    }
}


// Given an array of indices, calculate the cost to rebuild if all of them change
// You must call addRdeps and addCost first
function calcRebuildCosts(dat,xs) // Data{+rdeps+cost} -> [Int] -> Double
{
    var seen = {};
    var tot = 0;
    function f(i)
    {
        if (seen[i]) return;
        seen[i] = true;
        tot += dat[i].execution;
        var deps = dat[i].rdeps;
        for (var j = 0, n = deps.length; j < n; j++)
            f(deps[j]);
    }
    if (xs.length === 1 && dat[xs[0]].depends.length === 1)
        tot = dat[dat[xs[0]].depends[0]].cost + dat[xs[0]].execution;
    else
    {
        for (var i = 0, n = xs.length; i < n; i++)
            f(xs[i]);
    }
    return tot;
}


// Mutate the dat data, adding in cost, being the cost to rebuild if this item changes
function addCost(dat) // Data -> Mutate Data{+cost}
{
    for (var i = 0; i < dat.length; i++)
        dat[i].cost = calcRebuildCosts(dat, [i]);
}

function prepare(sum, dat) // Data -> DataEx
{
    addRdeps(dat);
    addCost(dat);

    function toHash(r){return typeof r === "string" ? "$" + r : "/" + r.source;}

    function findDirect(key)
    {
        var c = cache(toHash, function(r){
            var want = {};
            for (var i = 0; i < dat.length; i++)
            {
                if (testRegExp(r, dat[i].name))
                {
                    var deps = dat[i][key];
                    for (var j = 0; j < deps.length; j++)
                        want[deps[j]] = true;
                }
            }
            return want;
        });
        return function(i,r)
        {
            if (i in c(r))
                return true;
            else
                return false;
        };
    }

    function findTransitive(key, dirFwd)
    {
        var c = cache(toHash, function(r){
            var want = {};
            for (var i = 0; i < dat.length; i++)
            {
                var j = dirFwd ? i : dat.length - 1 - i;
                if ((j in want) || testRegExp(r,dat[j].name))
                {
                    want[j] = true;
                    var deps = dat[j][key];
                    for (var k = 0; k < deps.length; k++)
                        want[deps[k]] = true;
                }
            }
            return want;
        });
        return function(i,r){return i in c(r);};
    }

    return {original: dat
           ,summary: sum
           ,dependsOnThis: findDirect("rdeps")
           ,thisDependsOn: findDirect("depends")
           ,dependsOnThisTransitive: findTransitive("depends", false)
           ,thisDependsOnTransitive: findTransitive("rdeps", true)
           };
}


/////////////////////////////////////////////////////////////////////
// RULES

function ruleFilter(dat, query) // DataEx -> Query -> Dict String {items: [DataIndex], color: color}
{
    queryData = dat;
    var f = readQuery(query);
    var res = {};

    for (queryKey = 0; queryKey < dat.original.length; queryKey++)
    {
        queryVal = dat.original[queryKey];
        queryName = queryVal.name;
        queryGroup = null;
        queryBackColor = null;
        queryTextColor = null;
        if (f())
        {
            if (queryGroup === null) queryGroup = queryName;
            if (!(queryGroup in res))
                res[queryGroup] = {items: [queryKey], text: queryTextColor, back: queryBackColor};
            else
            {
                var c = res[queryGroup];
                c.items.push(queryKey);
                c.text = colorAnd(c.text, queryTextColor);
                c.back = colorAnd(c.back, queryBackColor);
            }
        }
    }
    return res;
}

function ruleTable(dat, query) // DataEx -> Query -> [Record]
{
    var res = ruleFilter(dat, query);
    var ans = [];
    for (var s in res)
    {
        var xs = res[s].items;
        var time = 0;
        var leaf = "";
        var unchanged = "";
        var run = 100000;
        for (var i = 0; i < xs.length; i++)
        {
            var x = dat.original[xs[i]];
            time += x.execution;
            leaf = bools(leaf, x.depends.length === 0);
            unchanged = bools(unchanged, x.changed !== x.built);
            run = Math.min(run,x.built);
        }
        ans.push({name:s, count:xs.length, time:time, back:res[s].back, text:res[s].text, cost:calcRebuildCosts(dat.original,xs), leaf:leaf, run:run, unchanged:unchanged});
    }
    return ans;
}

function ruleGraph(dat, query) // DataEx -> Query -> [Record]
{
    var res = ruleFilter(dat, query);

    var map = {}; // :: Dict Int [Int] -- which nodes a node lives at

    // loop through each value in res, putting it into map (these are parents)
    // for any not present, descend through the dat.original list, if you aren't included, add, if you are included, skip
    var direct = {};
    var ind = -1;
    for (var s in res)
    {
        ind++;
        var xs = res[s].items;
        for (var i = 0; i < xs.length; i++)
            direct[xs[i]] = ind;
    }
    function getDirect(key)
    {
        return key in direct ? [direct[key]] : [];
    }

    var indirect = {};
    function getIndirect(key)
    {
        if (key in indirect) return indirect[key];
        if (key in direct) return [];
        var ds = dat.original[key].depends;
        var res = [];
        for (var j = 0; j < ds.length; j++)
        {
            res.push(getIndirect(ds[j]));
            res.push(getDirect(ds[j]));
        }
        res = concatNub(res);
        indirect[key] = res;
        return res;
    }

    var ans = [];
    for (var s in res)
    {
        var xs = res[s].items;
        var ds = [];
        var is = [];
        for (var i = 0; i < xs.length; i++)
        {
            var depends = dat.original[xs[i]].depends;
            for (var j = 0; j < depends.length; j++)
            {
                ds.push(getDirect(depends[j]));
                is.push(getIndirect(depends[j]));
            }
        }
        ds = concatNub(ds);
        is = concatNub(is);
        ans.push({name:s, text:res[s].text, back:res[s].back, parents:ds, ancestors:is});
    }
    return ans;
}


/////////////////////////////////////////////////////////////////////
// COMMANDS

function commandFilter(last, dat, query) // DataEx -> Query -> Dict String [Trace]
{
    queryData = dat;
    var f = readQuery(query);
    var res = {};

    for (queryKey = 0; queryKey < dat.original.length; queryKey++)
    {
        queryVal = dat.original[queryKey];
        if (last && queryVal.built !== 0) continue;

        var val = {};
        for (var s in queryVal)
            val[s] = queryVal[s];
        var ts = queryVal.traces || [];
        queryVal = val;
        queryName = queryVal.name;
        queryBackColor = null;
        queryTextColor = null;
        for (var i = 0; i < ts.length; i++)
        {
            queryVal.traces = [ts[i]];
            queryGroup = null;
            if (f())
            {
                if (queryGroup === null) queryGroup = ts[i].command;
                if (!(queryGroup in res))
                    res[queryGroup] = {items: [ts[i]], text:queryTextColor, back:queryBackColor};
                else
                {
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

function commandTable(dat, query) // DataEx -> Query -> [Record]
{
    var res = commandFilter(false, dat, query);
    var ans = [];
    for (var s in res)
    {
        var xs = res[s].items;
        var time = 0;
        for (var i = 0; i < xs.length; i++)
            time += xs[i].stop - xs[i].start;
        ans.push({name:s, count:xs.length, text:res[s].text, back:res[s].back, time:time});
    }
    return ans;
}

function commandPlot(dat, query, buckets) // DataEx -> Query -> Int -> Dict String [Double]
{
    var end = dat.summary.maxTraceStopLast;
    var res = commandFilter(true, dat, query);
    var ans = {};
    for (var s in res)
    {
        var ts = res[s].items;
        var xs = [];
        for (var i = 0; i <= buckets; i++)
            xs.push(0); // fill with 1 more element, but the last bucket will always be 0

        for (var i = 0; i < ts.length; i++)
        {
            var start = ts[i].start * buckets / end;
            var stop = ts[i].stop * buckets / end;

            if (Math.floor(start) === Math.floor(stop))
                xs[Math.floor(start)] += stop - start;
            else
            {
                for (var j = Math.ceil(start); j < Math.floor(stop); j++)
                    xs[j]++;
                xs[Math.floor(start)] += Math.ceil(start) - start;
                xs[Math.floor(stop)] += stop - Math.floor(stop);
            }
        }
        ans[s] = {items: xs.slice(0,buckets), back: res[s].back || null};
    }
    return ans;
}


/////////////////////////////////////////////////////////////////////
// ENVIRONMENT

function readQuery(query)
{
    var f;
    try {
        f = new Function("return " + (query === "" ? "true" : query));
    } catch (e) {
        throw {user:true, name:"parse", query:query, message:e.toString()};
    }
    return function(){
        try {
            return f();
        } catch (e) {
            throw {user:true, name:"execution", query:query, message:e.toString()};
        }
    };
}


// These are global variables mutated/queried by query execution
var queryData = {};
var queryKey = 0;
var queryVal = {};
var queryName = "";
var queryGroup = null;
var queryBackColor = null;
var queryTextColor = null;

function childOf(r){return queryData.dependsOnThis(queryKey, r);}
function parentOf(r){return queryData.thisDependsOn(queryKey, r);}
function ancestorOf(r){return queryData.dependsOnThisTransitive(queryKey, r);}
function descendantOf(r){return queryData.thisDependsOnTransitive(queryKey, r);}
function descendentOf(r){return descendantOf(r);}

function /* export */ group(x)
{
    if (queryGroup === null) queryGroup = "";
    queryGroup += (queryGroup === "" ? "" : " ") + x;
    return true;
}

function backColor(c, b)
{
    if (b === undefined || b)
        queryBackColor = c;
    return true;
}

function textColor(c, b)
{
    if (b === undefined || b)
        queryTextColor = c;
    return true;
}

function rename(from, to)
{
    queryName = queryName.replace(from, to || "");
    return true;
}

function slowestRule()
{
    return queryData.summary.maxExecutionName;
}

function /* export */ leaf()
{
    return queryVal.depends.length === 0;
}

function /* export */ run(i)
{
    if (i === undefined)
        return queryVal.built;
    else
        return queryVal.built === i;
}

function /* export */ unchanged()
{
    return queryVal.changed !== queryVal.built;
}

function /* export */ name(r, groupName)
{
    if (r === undefined)
        return queryName;

    var res = execRegExp(r, queryName);
    if (res === null)
    {
        if (groupName === undefined)
            return false;
        else
        {
            group(groupName);
            return true;
        }
    }
    if (res.length !== 1)
    {
        for (var i = 1; i < res.length; i++)
            group(res[i]);
    }
    return true;
}

function /* export */ command(r, groupName)
{
    var n = (queryVal.traces || []).length;
    if (r === undefined)
        return n === 0 ? "" : queryVal.traces[0].command;

    for (var i = 0; i < n; i++)
    {
        var res = execRegExp(r, queryVal.traces[i].command);
        if (res === null)
            continue;
        if (res.length !== 1)
        {
            for (var j = 1; j < res.length; j++)
                group(res[j]);
        }
        return true;
    }
    if (groupName === undefined)
        return false;
    else
    {
        group(groupName);
        return true;
    }
}
