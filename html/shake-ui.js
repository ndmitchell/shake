/*jsl:option explicit*/
/*jsl:import shake-logic.js*/
"use strict";

// Report
//     {mode :: String
//     ,query :: String
//     ,sort :: String
//     ,sortRev :: Bool
//     }
//

var defaultMode = "summary";
var defaultQuery = "";
var defaultSort = "time";

var currentTable = null;
var currentPlot = null;


/////////////////////////////////////////////////////////////////////
// GLOBAL DATA

var shakeSummary = summary(shake);
var shakeEx = prepare(shakeSummary, shake);

var report = {}; // :: Report

function reportURL(report) // :: Report -> URL
{
    return "?mode=" + report.mode +
           (report.query === defaultQuery ? "" : "&query=" + escape(report.query).replace(/\+/g,"%2B")) +
           (report.sort === undefined || (!report.sortRev && report.sort === defaultSort) ? "" :
               "&sort=" + (report.sortRev ? "!" : "") + report.sort);
}

function urlReport() // :: IO Report
{
    var params = $.getParameters();
    var sort = params.sort || defaultSort;
    var sortRev = false;
    if (sort.substr(0,1) == "!")
    {
        sort = sort.substr(1);
        sortRev = true;
    }
    return {mode: params.mode || defaultMode
           ,query: params.query || defaultQuery
           ,sort: sort
           ,sortRev: sortRev
           };
}

function enteredReport()
{
    return {mode: $("#mode").val()
           ,query: $("#query").val()
           ,sort: report.sort
           ,sortRev: report.sortRev
           };
}

function setReport(r, replace, run)
{
    var changed = false;
    var report2 = recordUnion(r, report);
    $("#mode").val(report2.mode);
    $("#query").val(report2.query);
    $("#run").enable(false).attr("title", "The current query is displayed");
    if (recordEq(report,report2)) return;
    report = report2;

    if (window.history)
    {
        var title = report.mode + (report.query === "" ? "" : ": " + report.query);
        var url = reportURL(report);
        try {
            if (replace)
                window.history.replaceState(report, title, url);
            else
                window.history.pushState(report, title, url);
        } catch (e) {
            // Chrome disallows replaceState from origin null
        }
    }
    $("#link").attr("href", reportURL(report));

    if (run)
        runReport();
}


/////////////////////////////////////////////////////////////////////
// TABLE SHOWING

var rightAlign = {count:null, time:null, cost:null, run:null, leaf:null, unchanged:null};
var twoColumns = {cost:null, time:null};
var defaultRevSort = {run:null, name:null};

function tableSort(x)
{
    if (report.sort === x)
        setReport({sortRev: !report.sortRev}, true, false);
    else
        setReport({sort: x, sortRev: x in defaultRevSort}, true, false);
    showTable(currentTable);
}

function showTable(xs)
{
    currentTable = xs;
    if (xs.length === 0)
    {
        $("#output").html("No data found");
        return;
    }
    if (!(report.sort in xs[0]))
        setReport({sort:defaultSort, sortRev:false}, true, false);

    xs.sort(function(a,b){return (report.sortRev ? -1 : 1) * (b[report.sort] > a[report.sort] ? 1 : -1);});

    var res = "<table class='data'><tr class='header'>";
    for (var s in xs[0])
    {
        if (s === "back" || s === "text") continue;
        res += s in twoColumns ? "<td colspan='2' style='text-align:center;'" :
               s in rightAlign ? "<td style='text-align:right;'" :
               "<td";
        res += " onclick=\"tableSort('" + s + "')\">" + s;
        if (s === report.sort)
            res += " <span class='sort'>" + (report.sortRev ? "&#9650;" : "&#9660;") + "</span>";
        res += "</td>";
    }
    res += "</tr>";

    for (var i = 0; i < xs.length; i++)
    {
        var x = xs[i];
        res += "<tr";
        if (x.back) res += " style='background-color:" + x.back + ";'";
        if (x.text) res += " style='color:" + x.text + ";'";
        res += ">";
        for (var s in xs[0])
        {
            if (s === "text" || s === "back") continue;
            res += "<td" + (s in rightAlign ? " style='text-align:right;'" : "") + ">";
            if (s === "count")
                res += x[s] + " &times;";
            else if (s === "time" || s === "cost")
                res += showTime(x[s]) + "</td><td style='text-align:right;'>" + showPerc(x[s] / shakeSummary.sumExecution);
            else
                res += x[s];
            res += "</td>";
        }
        res += "</tr>";
    }
    res += "</table>";

    $("#output").html(res);
}


/////////////////////////////////////////////////////////////////////
// SHOW A PLOT

function showPlot(series, options)
{
    var $output = $("#output");
    var width = $output.width();
    var height = $output.height();

    if (series === null && options === null)
    {
        if (width === currentPlot.width && height === currentPlot.height)
            return;
        series = currentPlot.series;
        options = currentPlot.options;
    }
    currentPlot = {series:series, options:options, width:width, height:height};

    // Fudge factors to get it displaying nicely, seems Flot goes outside its bounds
    var div = $("<div>").width(width - 20).height(height - 10);
    $("#output").html("").append(div);
    $.plot(div, series, options);
}

window.onresize = function(){
    if (currentPlot !== null)
        showPlot(null, null);
};

/////////////////////////////////////////////////////////////////////
// RUNNING

function runReport()
{
    currentTable = null;
    currentPlot = null;

    try
    {
        switch(report.mode)
        {
        case "summary":
            var res = showSummary(shakeSummary);
            var s = $("#welcome").html();
            s += "<ul>";
            for (var i = 0; i < res.length; i++)
                s += "<li>" + res[i] + "</li>";
            s += "</ul>";
            s += "<p class='version'>Generated by <a href='http://shakebuild.com'>Shake " + version + "</a>.</p>";
            $("#output").html(s);
            break;

        case "cmd-plot":
            var xs = commandPlot(shakeEx, report.query, 100);
            var ys = [];
            for (var s in xs)
            {
                var x = xs[s].items;
                var data = [];
                for (var j = 0; j < x.length; j++)
                    data.push([j, x[j]]);
                ys.push({label:s, values:x, data:data, color:xs[s].back, avg:sum(x) / x.length});
            }
            if (ys.length === 0)
            {
                $("#output").html("No data found, " +
                    (shakeEx.summary.countTraceLast === 0
                        ? "there were no traced commands in the last run."
                        : "perhaps your filter is too restrictive?"));
            }
            else
            {
                ys.sort(function(a,b){return a.avg - b.avg;});
                showPlot(ys, {
                    legend: {show:true, position:"nw", sorted:"reverse"},
                    series: {stack:true, lines:{lineWidth:0,fill:1}},
                    yaxis: {min:0},
                    xaxis: {tickFormatter: function (i){return showTime(shakeSummary.maxTraceStopLast * i / 100);}}
                });
            }
            break;

        case "cmd-table":
            showTable(commandTable(shakeEx, report.query));
            break;

        case "rule-table":
            showTable(ruleTable(shakeEx, report.query));
            break;

        case "rule-graph":
            var xs = ruleGraph(shakeEx, report.query);
            if (xs.length > 250)
                $("#output").html("Viewing a graph with > 250 nodes is not supported, and you have " + xs.length + " nodes. Try grouping more aggressively");
            else if (typeof Viz === 'undefined')
                $("#output").html("Profile reports do not seem to have been built with GraphViz support, this feature is unavailable.");
            else
            {
                var res = "digraph \"\"{";
                res += "graph[nodesep=0.15,ranksep=0.3];";
                res += "node[fontname=\"sans-serif\",fontsize=9,penwidth=0.5,height=0,width=0];";
                res += "edge[penwidth=0.5,arrowsize=0.5];";
                for (var i = 0; i < xs.length; i++)
                {
                    res += "a" + i + "[label=\"" + xs[i].name.split("\\").join("\\\\").split("\"").join("\\\"") + "\"";
                    if (xs[i].back) res += ",style=filled,color=\"" + xs[i].back + "\"";
                    if (xs[i].text) res += ",fontcolor=\"" + xs[i].text + "\"";
                    res += "];";
                    var parents = xs[i].parents;
                    for (var j = 0; j < parents.length; j++)
                        res += "a" + i + "->a" + parents[j] + ";";
                    var ancestors = xs[i].ancestors;
                    for (var j = 0; j < ancestors.length; j++)
                        res += "a" + i + "->a" + ancestors[j] + "[style=dashed];";
                }
                res += "}";
                $("#output").html(Viz(res,"svg"));
            }
            break;

        case "help":
            $("#output").html($("#help").html());
            break;

        default:
            throw "Unknown report type: " + report.mode;
            break;
        }
    }
    catch (e)
    {
        if (!(e && e.user))
            throw e;
        $("#output").html($("#error").html());
        for (var s in e)
            $("#output ." + s).text(e[s]);
    }
}


/////////////////////////////////////////////////////////////////////
// STATE NAVIGATION

function example(mode,query)
{
    setReport({mode:mode, query:query, sortRev:false, sort:defaultSort}, false, true);
    return false;
}

$(function(){
    setReport(urlReport(), true, true);

    $("#mode,#query").bind("input change",function(){
        var mode = $("#mode").val();
        var query = $("#query").val();
        var enable = mode !== report.mode || query !== report.query;
        $("#run").enable(enable).attr("title", enable ? "" : "The current query is displayed");
        $("#link").attr("href", reportURL(enteredReport()));
    });

    $("#run").click(function(){
        setReport(enteredReport(), false, true);
    });

    window.onpopstate = function (e){
        setReport(urlReport(), true, true);
    };
});

/////////////////////////////////////////////////////////////////////
// TEMPLATES

$(function(){
    $("a.example").each(function(){
        var mode = $(this).attr("data-mode");
        var query = $(this).attr("data-query");
        if (query === undefined) query = $(this).text();
        var href = reportURL({mode:mode, query:query});
        var onclick = "return example(unescape('" + escape(mode) + "'),unescape('" + escape(query) + "'));";
        $(this).attr("href", href).attr("target","_blank")[0].setAttribute("onclick",onclick);
    });

    $("a.shake").each(function(){
        var href = "http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html#v:" +
                   $(this).text().replace("'","-39-");
        $(this).attr("href", href).attr("target","_blank");
    });
});
