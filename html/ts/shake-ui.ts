/*jsl:option explicit*/
/*jsl:import shake-logic.js*/
"use strict";

var prepared = prepare(profile);
var currentTable: MapString<any>[] = null;

/////////////////////////////////////////////////////////////////////
// REPORT

class Report {
    mode: string;
    query: string;
    sort: string = "time";
    sortRev: boolean = false;

    constructor(mode_ = "summary", query_ = "") {
        this.mode = mode_; this.query = query_;
    }
}

var report: Report = new Report(null, null);

function reportEq(r1: Report, r2: Report): boolean {
    return r1.mode === r2.mode && r1.query === r2.query && r1.sort === r2.sort && r1.sortRev === r2.sortRev;
}

function reportToURL(r: Report): string // Get the URL
{
    const def = new Report();
    return "?mode=" + r.mode +
        (r.query === def.query ? "" : "&query=" + encodeURI(r.query).replace(/\+/g, "%2B")) +
        ((!r.sortRev && r.sort === def.sort) ? "" :
        "&sort=" + (r.sortRev ? "!" : "") + r.sort);
}

function reportFromURL(s = window.location.search): Report
{
    var res = new Report();
    const params = uriQueryParameters(s);
    if ("mode" in params)
        res.mode = params["mode"];
    if ("query" in params)
        res.query = params["query"];
    if ("sort" in params) {
        const sort = params["sort"];
        res.sortRev = sort.substr(0, 1) == "!";
        res.sort = sort.substr(res.sortRev ? 1 : 0);
    }
    return res;
}

function reportFromUser() : Report
{
    return new Report($("#mode").val(), $("#query").val());
}

function setReport(set: (r : Report) => Report, replace: boolean, run: boolean)
{
    const report2 = set(recordCopy(report));
    $("#mode").val(report2.mode);
    $("#query").val(report2.query);
    $("#run").enable(false).attr("title", "The current query is displayed");
    if (reportEq(report,report2)) return;
    report = report2;

    if (window.history)
    {
        var title = report.mode + (report.query === "" ? "" : ": " + report.query);
        var url = reportToURL(report);
        try {
            if (replace)
                window.history.replaceState(report, title, url);
            else
                window.history.pushState(report, title, url);
        } catch (e) {
            // Chrome disallows replaceState from origin null
        }
    }
    $("#link").attr("href", reportToURL(report));

    if (run)
        runReport();
}


/////////////////////////////////////////////////////////////////////
// TABLE SHOWING

var rightAlign : MapString<void> = {count:null, time:null, cost:null, run:null, leaf:null, unchanged:null};
var twoColumns : MapString<void> = {cost:null, time:null};
var defaultRevSort : MapString<void> = {run:null, name:null};

function tableSort(x: string): void
{
    if (report.sort === x)
        setReport(r => { r.sortRev = !r.sortRev; return r}, true, false);
    else
        setReport(r => { r.sort = x; r.sortRev = x in defaultRevSort; return r}, true, false);
    showTable(currentTable);
}

function showTable(xs: MapString<any>[]): void
{
    currentTable = xs;
    if (xs.length === 0)
    {
        $("#output").html("No data found");
        return;
    }
    if (!(report.sort in xs[0]))
        setReport(r => new Report(r.mode, r.query), true, false);

    xs.sort(function(a,b){return (report.sortRev ? -1 : 1) * ((<any>b)[report.sort] > (<any>a)[report.sort] ? 1 : -1);});

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

    for (const x of xs)
    {
        res += "<tr";
        if (x["back"]) res += " style='background-color:" + x["back"] + ";'";
        if (x["text"]) res += " style='color:" + x["text"] + ";'";
        res += ">";
        for (var s in xs[0])
        {
            if (s === "text" || s === "back") continue;
            res += "<td" + (s in rightAlign ? " style='text-align:right;'" : "") + ">";
            if (s === "count")
                res += x[s] + " &times;";
            else if (s === "time" || s === "cost")
                res += showTime(x[s]) + "</td><td style='text-align:right;'>" + showPerc(x[s] / prepared.summary.sumExecution);
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

module jquery.flot {
    interface seriesOptions {
        // We use the flot stack plugin
    }
}

type dataSeries = jquery.flot.dataSeries
type plotOptions = jquery.flot.plotOptions

var currentPlot: { series: dataSeries[], options: plotOptions, width: number, height: number } = null;

function showPlot(series: dataSeries[], options: plotOptions) : void
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

// Provided by Viz.js, which is not always required
declare function Viz(dot: string, typ: string): string;

function runReport()
{
    currentTable = null;
    currentPlot = null;

    try
    {
        switch(report.mode)
        {
        case "summary":
            const res = showSummary(prepared.summary);
            let s = $("#welcome").html();
            s += "<ul>";
            for (var i = 0; i < res.length; i++)
                s += "<li>" + res[i] + "</li>";
            s += "</ul>";
            s += "<p class='version'>Generated by <a href='http://shakebuild.com'>Shake " + version + "</a>.</p>";
            $("#output").html(s);
            break;

        case "cmd-plot": {
            const xs = commandPlot(prepared, report.query, 100);
            const ys: (dataSeries & { avg: number })[] = [];
            for (const s in xs) {
                var x = xs[s].items;
                var data: [number, number][] = [];
                for (var j = 0; j < x.length; j++)
                    data.push([j, x[j]]);
                ys.push({ label: s, /* values:x, */ data: data, color: xs[s].back, avg: sum(x) / x.length });
            }
            if (ys.length === 0) {
                $("#output").html("No data found, " +
                    (prepared.summary.countTraceLast === 0
                        ? "there were no traced commands in the last run."
                        : "perhaps your filter is too restrictive?"));
            }
            else {
                ys.sort(function (a, b) { return a.avg - b.avg; });
                showPlot(ys, {
                    legend: { show: true, position: "nw", sorted: "reverse" },
                    series: { stack: true, lines: { lineWidth: 0, fill: 1 } },
                    yaxis: { min: 0 },
                    xaxis: { tickFormatter: function (i) { return showTime(prepared.summary.maxTraceStopLast * i / 100); } }
                });
            }
            } break;

        case "cmd-table":
            showTable(commandTable(prepared, report.query));
            break;

        case "rule-table":
            showTable(ruleTable(prepared, report.query));
            break;

        case "rule-graph": {
            const xs = ruleGraph(prepared, report.query);
            if (xs.length > 250)
                $("#output").html("Viewing a graph with > 250 nodes is not supported, and you have " + xs.length + " nodes. Try grouping more aggressively");
            else if (typeof Viz === 'undefined')
                $("#output").html("Profile reports do not seem to have been built with GraphViz support, this feature is unavailable.");
            else {
                let res = "digraph \"\"{";
                res += "graph[nodesep=0.15,ranksep=0.3];";
                res += "node[fontname=\"sans-serif\",fontsize=9,penwidth=0.5,height=0,width=0];";
                res += "edge[penwidth=0.5,arrowsize=0.5];";
                for (var i = 0; i < xs.length; i++) {
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
                $("#output").html(Viz(res, "svg"));
            }
            }break;

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

function example(mode: string, query: string): boolean
{
    setReport(_ => new Report(mode, query), false, true);
    return false;
}

function initProfile() {
    $(function () {
        setReport(_ => reportFromURL(), true, true);

        $("#mode,#query").bind("input change", function () {
            var mode = $("#mode").val();
            var query = $("#query").val();
            var enable = mode !== report.mode || query !== report.query;
            $("#run").enable(enable).attr("title", enable ? "" : "The current query is displayed");
            $("#link").attr("href", reportToURL(reportFromUser()));
        });

        $("#run").click(function () {
            setReport(_ => reportFromUser(), false, true);
        });
        $("#query").keypress(function (e) {
            if (e.which == 13) $("#run").click();
        });

        window.onpopstate = function (e) {
            setReport(_ => reportFromUser(), true, true);
        };

        $("a.example").each(function () {
            var mode = $(this).attr("data-mode");
            var query = $(this).attr("data-query");
            if (query === undefined) query = $(this).text();
            var href = reportToURL(new Report(mode, query));
            var onclick = "return example(decodeURI('" + encodeURI(mode) + "'),decodeURI('" + encodeURI(query) + "'));";
            $(this).attr("href", href).attr("target", "_blank")[0].setAttribute("onclick", onclick);
        });

        $("a.shake").each(function () {
            var href = "http://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html#v:" +
                $(this).text().replace("'", "-39-");
            $(this).attr("href", href).attr("target", "_blank");
        });
    });
}
