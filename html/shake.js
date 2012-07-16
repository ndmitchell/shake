/*jsl:option explicit*/
"use strict";

//////////////////////////////////////////////////////////////////////
// UTILITIES

function showTime(x){return x.toFixed(2) + "s";}
function showPerc(x){return (x*100).toFixed(2) + "%";}
function showShort(x){return x;}

function plural(n,not1,is1){
    return n === 1
        ? (is1 === undefined ? "" : is1)
        : (not1 === undefined ? "s" : not1);
}


function listEq(xs, ys)
{
    if (xs.length !== ys.length) return false;
    for (var i = 0; i < xs.length; i++)
    {
        if (xs[i] !== ys[i])
            return false;
    }
    return true;
}

// Return the n top entries, grouping as necessary by words
// huffman :: Int -> [{sum :: Double, name :: String}] -> [{sum :: Double, count :: Int, name :: String}]
function huffman(resultSize, xs)
{
    var val = {sum: 0, count: 0, children: {}};

    for (var i = 0; i < xs.length; i++)
    {
        var val2 = val;
        var ys = xs[i].name.split(' ');
        for (var j = 0; j < ys.length; j++)
        {
            if (val2.children[ys[j]])
                val2 = val2.children[ys[j]];
            else
            {
                var t = {sum: 0, count: 0, children: {}};
                val2.children[ys[j]] = t;
                val2 = t;
            }
            val2.sum += xs[i].sum;
            val2.count++;
        }
    }

    function compress(val) // return the number of items
    {
        var n = 0;
        for (var j in val.children)
            n += compress(val.children[j]);
        if (n === 1)
            val.children = {};
        return Math.max(1,n);
    }
    compress(val);

    function flatten(path, val, res)
    {
        for (var i in val.children)
        {
            var ii = val.children[i];
            var pp = path.slice(0);
            pp.push(i);
            res.push({sum:ii.sum, count:ii.count, name:pp, free:path.length===0});
            flatten(pp,ii,res);
        }
    }
    var flat = [];
    flatten([], val, flat);

    while(true)
    {
        // order everything
        flat.sort(function(a,b){var i = b.sum - a.sum; return i !== 0 ? i : b.count - a.count;});

        // now, if something in the first n is not free, mark it free and delete anyone who relies on it
        // then repeat, if nothing is not free, break
        var cont = false;
        for (var i = 0; i < Math.min(resultSize, flat.length); i++)
        {
            if (!flat[i].free)
            {
                cont = true;
                flat[i].free = true;
                for (var j = 0; j < flat.length; j++)
                {
                    if (listEq(flat[i].name.slice(0,flat[i].name.length-1), flat[j].name))
                    {
                        flat[j].count -= flat[i].count;
                        flat[j].sum -= flat[i].sum;
                    }
                }
                break;
            }
        }
        if (!cont) break;
    }

    for (var i = 0; i < flat.length; i++)
        flat[i].name = flat[i].name.join(" ");
    return flat.slice(0, resultSize);
}


//////////////////////////////////////////////////////////////////////
// DATA MANIPULATION/ADDITION

// which runs have built data in them, sorted
// built :: [Int]
var built = function(){
    var seen = {};
    for (var i = 0; i < shake.length; i++)
        seen[shake[i].built] = true;
    var seen2 = [];
    for (var i in seen)
        seen2.push(Number(i));
    return seen2.sort(function cmp(a,b){return a-b;});
}();

// what is the index of the last build run
// lastRun :: Int
var lastRun = built.length === 0 ? 0 : built[built.length-1];

// Summary statistics
var countLast = 0; // :: Int, number of rules run in 'lastRun'
var sumExecution = 0; // :: Seconds, build time in total
var maxExecution = 0; // :: Seconds, longest build rule
var countTrace = 0, countTraceLast = 0; // :: Int, traced commands run
var sumTrace = 0; // :: Seconds, time running traced commands
var maxTrace = 0; // :: Seconds, lonest traced command
var maxTraceStopLast = 0; // :: Seconds, time the last traced command stopped

var _ = function(){
    // Fold over shake to produce the summary
    for (var i = 0; i < shake.length; i++)
    {
        var isLast = shake[i].built === lastRun;
        countLast += isLast ? 1 : 0;
        sumExecution += shake[i].execution;
        maxExecution = Math.max(maxExecution, shake[i].execution);
        var traces = shake[i].traces;
        if (!traces) continue;
        for (var j = 0; j < traces.length; j++)
        {
            countTrace += 1;
            countTraceLast += isLast ? 1 : 0;
            sumTrace += traces[j].stop - traces[j].start;
            maxTrace = Math.max(maxTrace, traces[j].stop - traces[j].start);
            maxTraceStopLast = Math.max(maxTraceStopLast, isLast ? traces[j].stop : 0);
        }
    }
}();


// Mutate the shake data, adding in rdeps, being the 1-level reverse dependencies
function addRdeps()
{
    // find the reverse dependencies
    var rdeps = [];
    for (var i = 0; i < shake.length; i++)
        rdeps[i] = {};
    for (var i = 0; i < shake.length; i++)
    {
        var deps = shake[i].depends;
        for (var j = 0, n = deps.length; j < n; j++)
            rdeps[deps[j]][i] = true;
    }
    for (var i = 0; i < rdeps.length; i++)
    {
        var ans = [];
        for (var j in rdeps[i])
            ans.push(Number(j));
        shake[i].rdeps = ans;
    }
}


// Given an array of indices, calculate the cost to rebuild if all of them change
// You must call addRdeps and addCost first
function calcRebuildCosts(xs)
{
    var seen = {};
    var tot = 0;
    function f(i)
    {
        if (seen[i]) return;
        seen[i] = true;
        tot += shake[i].execution;
        var deps = shake[i].rdeps;
        for (var j = 0, n = deps.length; j < n; j++)
            f(deps[j]);
    }
    if (xs.length === 1 && shake[xs[0]].depends.length === 1)
        tot = shake[shake[xs[0]].depends[0]].cost + shake[xs[0]].execution;
    else
    {
        for (var i = 0, n = xs.length; i < n; i++)
            f(xs[i]);
    }
    return tot;
}


// Mutate the shake data, adding in cost, being the cost to rebuild if this item changes
function addCost()
{
    for (var i = 0; i < shake.length; i++)
        shake[i].cost = calcRebuildCosts([i]);
}


//////////////////////////////////////////////////////////////////////
// REPORTS

function reportSummary()
{
    var res =
        "<ul>" +
        "<li><strong>Runs:</strong> This database has tracked " + built.length + " run" + plural(built.length) + ".</li>" +
        "<li><strong>Rules:</strong> There are " + shake.length + " rules (" + countLast + " rebuilt in the last run).</li>" +
        "<li><strong>Commands:</strong> Building required " + countTrace + " traced commands (" + countTraceLast + " in the last run).</li>" +
        "<li><strong>Build time:</strong> The total (unparallelised) build time is " + showTime(sumExecution) + " of which " + showTime(sumTrace) + " is traced commands.</li>" +
        "<li><strong>Longest steps:</strong> The longest rule takes " + showTime(maxExecution) + ", and the longest traced command takes " + showTime(maxTrace) + ".</li>" +
        "<li><strong>Parallelism:</strong> Last run gave an average parallelism of " + (sumTrace / maxTraceStopLast).toFixed(2) + " times over " + showTime(maxTraceStopLast) + ".</li>" +
        "</ul>";
    return res;
}


function reportParallelismGraph()
{
    var buckets = [];
    var countBuckets = 100;
    for (var i = 0; i <= countBuckets; i++)
        buckets.push(0); // fill with 1 more element, but the last bucket will always be 0

    for (var i = 0; i < shake.length; i++)
    {
        var traces = shake[i].traces;
        if (!traces || shake[i].built !== lastRun) continue;
        for (var j = 0; j < traces.length; j++)
        {
            var start = traces[j].start * countBuckets / maxTraceStopLast;
            var stop = traces[j].stop * countBuckets / maxTraceStopLast;

            if (Math.floor(start) === Math.floor(stop))
                buckets[Math.floor(start)] += stop - start;
            else
            {
                for (var k = Math.ceil(start); k < Math.floor(stop); k++)
                    buckets[k]++;
                buckets[Math.floor(start)] += Math.ceil(start) - start;
                buckets[Math.floor(stop)] += stop - Math.floor(stop);
            }
        }
    }
    var maxBucket = 0;
    for (var i = 0; i < countBuckets; i++)
        maxBucket = Math.max(maxBucket, buckets[i]);
    maxBucket = Math.ceil(maxBucket - 0.00001);

    var plotvals = [];
    for (var i = 0; i < countBuckets; i++)
        plotvals.push([i, (buckets[i] * 100 / maxBucket)]);
    return [{color: '#5EB95E', data: plotvals}];
}


function reportExpensiveRules()
{
    var top = shake.slice(0).sort(function(a,b){return b.execution-a.execution;}).slice(0,15);
    var res = "";
    for (var i = 0; i < top.length; i++)
    {
        res += "<tr>" +
            "<td><div class='progress progress-success' style='height: 10px'>" +
            "<div class='bar' style='width:" + (top[i].execution * 40 / top[0].execution) + "px;'></div></div></td>" +
            "<td>" + showTime(top[i].execution) + "</td>" +
            "<td>" + showPerc(top[i].execution / sumExecution) + "</td>" +
            "<td>" + top[i].name + "</td>" +
            "</tr>";
    }
    return res;
}


function reportExpensiveCommands()
{
    var tooList = [];
    for (var i = 0; i < shake.length; i++)
    {
        var traces = shake[i].traces;
        if (!traces) continue;
        for (var j = 0; j < traces.length; j++)
            tooList.push({name: traces[j].command, sum: traces[j].stop - traces[j].start});
    }

    var toolList = huffman(15, tooList);
    var res = "";
    for (var i = 0; i < toolList.length; i++)
    {
        res += "<tr>" +
            "<td><div class='progress progress-success' style='height: 10px'>" +
            "<div class='bar' style='width:" + (toolList[i].sum * 40 / toolList[0].sum) + "px;'></div></div></td>" +
            "<td>" + showTime(toolList[i].sum) + "</td>" +
            "<td>" + showPerc(toolList[i].sum / sumExecution) + "</td>" +
            "<td>" + toolList[i].count + "&nbsp;&times;</td>" +
            "<td>" + toolList[i].name + "</td>" +
            "</tr>";
    }
    return res;
}


function reportRebuildCost()
{
    $("#rebuild-cost input").live('input', function(){
        try {
            var regex = new RegExp($(this).val());
            $(this).parent().removeClass("error").find("span").text("");
        } catch(e) {
            $(this).parent().addClass("error").find("span").text(e);
            return;
        }
        var result1 = [];
        var resultN = {};
        for (var i = 0; i < shake.length; i++)
        {
            var res = regex.exec(shake[i].name);
            if (res === null) continue;
            if (res.length === 1)
                result1.push(shake[i]);
            else
            {
                var extra = "";
                for (var j = 1; j < res.length; j++)
                    extra += (extra === "" ? "" : " ") + res[j];
                if (resultN[extra] === undefined)
                    resultN[extra] = {name: extra, deps: {}, count: 0};
                resultN[extra].deps[i] = true;
                resultN[extra].count++;
            }
        }
        for (var i in resultN)
        {
            var xs = [];
            for (var j in resultN[i].deps)
                xs.push(Number(j));
            result1.push({name: resultN[i].name + " (" + resultN[i].count + ")", cost: calcRebuildCosts(xs)});
        }
        var top = result1.sort(function(a,b){return b.cost - a.cost;}).slice(0, 15);

        var res = "";
        for (var i = 0; i < top.length; i++)
        {
          res += "<tr>" +
            "<td><div class='progress progress-success' style='height: 10px'>" +
            "<div class='bar' style='width:" + (top[i].cost * 40 / top[0].cost) + "px;'></div></div></td>" +
            "<td>" + showTime(top[i].cost) + "</td>" +
            "<td>" + showPerc(top[i].cost / sumExecution) + "</td>" +
            "<td>" + top[i].name + "</td></tr>";
        }
        $("#rebuild-details tbody").empty().append(res);
    });

    // Put them in order
    var top = shake.slice(0).sort(function(a,b){return b.cost-a.cost;}).slice(0,15);

    var res = "";
    for (var i = 0; i < top.length; i++)
    {
      res += "<tr>" +
        "<td><div class='progress progress-success' style='height: 10px'>" +
        "<div class='bar' style='width:" + (top[i].cost * 40 / top[0].cost) + "px;'></div></div></td>" +
        "<td>" + showTime(top[i].cost) + "</td>" +
        "<td>" + showPerc(top[i].cost / sumExecution) + "</td>" +
        "<td>" + top[i].name + "</td></tr>";
    }
    return res;
}


//////////////////////////////////////////////////////////////////////
// LOADING

function load()
{
    $('#summary').append(reportSummary());
    $.plot($('#shakeplot'), reportParallelismGraph());
    $("#rule-details tbody").append(reportExpensiveRules());
    $("#cmd-details tbody").append(reportExpensiveCommands());
    addRdeps();
    addCost();
    $("#rebuild-details tbody").append(reportRebuildCost());
}


function reportUnchanged()
{
    for (var i = 0; i < shake.length; i++)
    {
        if (shake[i].changed !== shake[i].built)
            console.log(shake[i].name);
    }
}
