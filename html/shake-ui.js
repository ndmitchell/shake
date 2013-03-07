/*jsl:option explicit*/
"use strict";

jQuery.fn.enable = function (x)
{
    // Set the values to enabled/disabled
    return this.each(function () {
        if (x)
            $(this).removeAttr('disabled');
        else
            $(this).attr('disabled','disabled');
    });
};


var shakeSummary;
var shakeEx;

var rightAlign = {count:null, time:null, cost:null, runs:null, leaf:null, unchanged:null};
var twoColumns = {cost:null, time:null};

var lastTable;
var lastSort;
var lastOrder;

function tableSort(x)
{
    if (x === lastSort)
        showTable(lastTable, x, lastOrder * -1);
    else
        showTable(lastTable, x, 1);
}

function showTable(xs, key, order)
{
    lastTable = xs;
    lastSort = key === undefined ? "time" : key;
    lastOrder = order === undefined ? 1 : order;

    if (xs.length === 0)
    {
        $("#output").html("No data found");
        return;
    }
    xs.sort(function(a,b){return lastOrder * (b[lastSort] > a[lastSort] ? 1 : -1);});

    var res = "<table class='data'><tr class='header'>";
    for (var s in xs[0])
    {
        res += s in twoColumns ? "<td colspan='2' style='text-align:center;'" :
               s in rightAlign ? "<td style='text-align:right;'" :
               "<td";
        res += " onclick=\"tableSort('" + s + "')\">" + s;
        if (s === lastSort)
            res += " <span class='sort'>" + (lastOrder > 0 ? "&#9660;" : "&#9650") + "</span>";
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

function run(mode, query)
{
    switch(mode)
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
        var xs = commandPlot(shakeEx, query, 100);
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
        var $output = $("#output");
        // Fudge factors to get it displaying nicely, seems Flot goes outside its bounds
        var div = $("<div>").width($output.width() - 20).height($output.height() - 3);
        $("#output").html("").append(div);
        $.plot(div, ys, {
            legend: {show:true, position:"nw"},
            series: {stack:true, lines:{lineWidth:0,fill:1}},
            yaxis: {min:0},
            xaxis: {tickFormatter: function (i){return showTime(shakeSummary.maxTraceStopLast * i / 100)}}
        });
        break;

    case "cmd-table":
        showTable(commandTable(shakeEx, query));
        break;

    case "rule-table":
        showTable(ruleTable(shakeEx, query));
        break;

    case "examples": case "help":
        $("#output").html($("#" + mode).html());
        break;
    }
}

$(function(){
    shakeSummary = summary(shake);
    shakeEx = prepare(shakeSummary, shake);

    var lastMode = $("#mode").val();
    var lastQuery = $("#query").val();

    var q = parseQuery();
    if (q.mode) $("#mode").val(q.mode);
    if (q.query) $("#query").val(q.query);

    function upd()
    {
        var mode = $("#mode").val();
        var query = $("#query").val();
        var enable = mode !== lastMode || query !== lastQuery;
        $("#run").enable(enable).attr("title", enable ? "" : "The current query is displayed");
        $("#link").attr("href", "?mode=" + escape_(mode) + "&query=" + escape_(query));
    }

    $("#run").click(function(){
        lastMode = $("#mode").val();
        lastQuery = $("#query").val();
        upd();
        run(lastMode, lastQuery);
    });

    $("#mode,#query").bind("input change",upd);
    $("#run").click();

    $("a.example").each(function(){
        var mode = $(this).attr("mode");
        var query = $(this).text();
        var href = "?mode=" + escape_(mode) + "&query=" + escape_(query);
        var onclick = "return example(unescape('" + escape_(mode) + "'),unescape('" + escape_(query) + "'));";
        $(this).attr("href", href).attr("target","_blank")[0].setAttribute("onclick",onclick);
    });

    $("a.shake").each(function(){
        var href = "http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html#v:" +
                   $(this).text().replace("'","-39-");
        $(this).attr("href", href).attr("target","_blank");
    });
});

function escape_(x)
{
    return escape(x).replace("+","%2B");
}

function example(mode,query)
{
    $("#mode").val(mode);
    $("#query").val(query);
    $("#run").click();
    return false;
}

function parseQuery() // :: IO (Dict String String)
{
    // From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    var params = {},
        e,
        a = /\+/g,  // Regex for replacing addition symbol with a space
        r = /([^&=]+)=?([^&]*)/g,
        d = function (s) { return decodeURIComponent(s.replace(a, " ")); },
        q = window.location.search.substring(1);

    while (e = r.exec(q))
        params[d(e[1])] = d(e[2]);

    return params;
}
