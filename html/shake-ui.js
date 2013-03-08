/*jsl:option explicit*/
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
           (report.query === defaultQuery ? "" : "&query=" + escape(report.query).replace("+","%2B")) +
           (!report.sortRev && report.sort === defaultSort ? "" : "&sort=" + (report.sortRev ? "!" : "") + report.sort);
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
        if (replace)
            window.history.replaceState(report, title, url);
        else
            window.history.pushState(report, title, url);
    }
    $("#link").attr("href", reportURL(report));

    if (run)
        runReport();
}


/////////////////////////////////////////////////////////////////////
// TABLE SHOWING

var rightAlign = {count:null, time:null, cost:null, runs:null, leaf:null, unchanged:null};
var twoColumns = {cost:null, time:null};

function tableSort(x)
{
    if (report.sort === x)
        setReport({sortRev: !report.sortRev}, true, false);
    else
        setReport({sort: x, sortRev: false}, true, false);
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
        res += "<tr>";
        for (var s in xs[0])
        {
            res += "<td" + (s in rightAlign ? " style='text-align:right;'" : "") + ">";
            if (s === "count")
                res += x[s] + " &times;"
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

    switch(report.mode)
    {
    case "summary":
        var res = showSummary(shakeSummary);
        var s = $("#welcome").html();
        s += "<ul>";
        for (var i = 0; i < res.length; i++)
            s += "<li>" + res[i] + "</li>";
        s += "</ul>";
        $("#output").html(s);
        break;

    case "cmd-plot":
        var xs = commandPlot(shakeEx, report.query, 100);
        var ys = [];
        for (var s in xs)
        {
            var x = xs[s];
            var data = [];
            for (var j = 0; j < x.length; j++)
                data.push([j, x[j]]);
            ys.push({label:s, values:x, data:data, avg:sum(x) / x.length});
        }
        ys.sort(function(a,b){return a.avg - b.avg;});
        showPlot(ys, {
            legend: {show:true, position:"nw"},
            series: {stack:true, lines:{lineWidth:0,fill:1}},
            yaxis: {min:0},
            xaxis: {tickFormatter: function (i){return showTime(shakeSummary.maxTraceStopLast * i / 100)}}
        });
        break;

    case "cmd-table":
        showTable(commandTable(shakeEx, report.query));
        break;

    case "rule-table":
        showTable(ruleTable(shakeEx, report.query));
        break;

    case "help":
        $("#output").html($("#help").html());
        break;
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
        var mode = $(this).attr("mode");
        var query = $(this).text();
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
