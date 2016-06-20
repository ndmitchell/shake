// GENERATED CODE - DO NOT MODIFY
// SOURCE IS IN THE ts/ DIRECTORY

"use strict";
var Summary = (function () {
    function Summary() {
        this.count = 0;
        this.countLast = 0;
        this.highestRun = 0;
        this.sumExecution = 0;
        this.maxExecution = 0;
        this.maxExecutionName = "";
        this.countTrace = 0;
        this.countTraceLast = 0;
        this.sumTrace = 0;
        this.sumTraceLast = 0;
        this.maxTrace = 0;
        this.maxTraceName = "";
        this.maxTraceStopLast = 0;
    }
    return Summary;
})();
function summary(dat) {
    var res = new Summary();
    res.count = dat.length;
    for (var _i = 0; _i < dat.length; _i++) {
        var e = dat[_i];
        var isLast = e.built === 0;
        res.countLast += isLast ? 1 : 0;
        res.sumExecution += e.execution;
        res.maxExecution = Math.max(res.maxExecution, e.execution);
        if (res.maxExecution === e.execution)
            res.maxExecutionName = e.name;
        res.highestRun = Math.max(res.highestRun, e.changed);
        var traces = e.traces;
        if (!traces)
            continue;
        for (var _a = 0; _a < traces.length; _a++) {
            var t_1 = traces[_a];
            var time = t_1.stop - t_1.start;
            res.countTrace += 1;
            res.countTraceLast += isLast ? 1 : 0;
            res.sumTrace += time;
            res.sumTraceLast += isLast ? time : 0;
            res.maxTrace = Math.max(res.maxTrace, time);
            if (res.maxTrace == time)
                res.maxTraceName = t_1.command;
            res.maxTraceStopLast = Math.max(res.maxTraceStopLast, isLast ? t_1.stop : 0);
        }
    }
    return res;
}
function showSummary(sum) {
    return ["This database has tracked " + (sum.highestRun + 1) + " run" + plural(sum.highestRun + 1) + ".",
        "There are " + sum.count + " rules (" + sum.countLast + " rebuilt in the last run).",
        "Building required " + sum.countTrace + " traced commands (" + sum.countTraceLast + " in the last run).",
        "The total (unparallelised) build time is " + showTime(sum.sumExecution) + " of which " + showTime(sum.sumTrace) + " is traced commands.",
        "The longest rule takes " + showTime(sum.maxExecution) + " (" + sum.maxExecutionName + ") and the longest traced command takes " + showTime(sum.maxTrace) + " (" + sum.maxTraceName + ").",
        "Last run gave an average parallelism of " + (sum.maxTraceStopLast === 0 ? 0 : sum.sumTraceLast / sum.maxTraceStopLast).toFixed(2) + " times over " + showTime(sum.maxTraceStopLast) + "."
    ];
}
var Prepare = (function () {
    function Prepare() {
    }
    return Prepare;
})();
function addRdeps(dat) {
    var rdeps = [];
    for (var i = 0; i < dat.length; i++)
        rdeps[i] = {};
    for (var i = 0; i < dat.length; i++) {
        for (var _i = 0, _a = dat[i].depends; _i < _a.length; _i++) {
            var j = _a[_i];
            rdeps[j][i] = null;
        }
    }
    var res = dat;
    for (var i = 0; i < rdeps.length; i++) {
        var ans = [];
        for (var j in rdeps[i])
            ans.push(Number(j));
        res[i].rdeps = ans;
    }
    return res;
}
function calcRebuildCosts(dat, xs) {
    var seen = {};
    var tot = 0;
    function f(i) {
        if (i in seen)
            return;
        seen[i] = null;
        tot += dat[i].execution;
        for (var _i = 0, _a = dat[i].rdeps; _i < _a.length; _i++) {
            var j = _a[_i];
            f(j);
        }
    }
    if (xs.length === 1 && dat[xs[0]].depends.length === 1)
        tot = dat[dat[xs[0]].depends[0]].cost + dat[xs[0]].execution;
    else {
        for (var _i = 0; _i < xs.length; _i++) {
            var x = xs[_i];
            f(x);
        }
    }
    return tot;
}
function addCost(dat) {
    var res = dat;
    for (var i = 0; i < dat.length; i++) {
        res[i].cost = calcRebuildCosts(res, [i]);
    }
    return res;
}
function prepare(dat_) {
    var sum = summary(dat_);
    var dat = addCost(addRdeps(dat_));
    function toHash(r) {
        return typeof r === "string" ? "$" + r : "/" + r.source;
    }
    function findDirect(key) {
        var c = cache(toHash, function (r) {
            var want = {};
            for (var _i = 0; _i < dat.length; _i++) {
                var e = dat[_i];
                if (testRegExp(r, e.name)) {
                    var deps = (e)[key];
                    for (var _a = 0; _a < deps.length; _a++) {
                        var j = deps[_a];
                        want[j] = null;
                    }
                }
            }
            return want;
        });
        return function (i, r) { return i in c(r); };
    }
    function findTransitive(key, dirFwd) {
        var c = cache(toHash, function (r) {
            var want = {};
            for (var i = 0; i < dat.length; i++) {
                var j = dirFwd ? i : dat.length - 1 - i;
                if ((j in want) || testRegExp(r, dat[j].name)) {
                    want[j] = null;
                    var deps = (dat[j])[key];
                    for (var _i = 0; _i < deps.length; _i++) {
                        var k = deps[_i];
                        want[k] = null;
                    }
                }
            }
            return want;
        });
        return function (i, r) { return i in c(r); };
    }
    return {
        original: dat,
        summary: sum,
        dependsOnThis: findDirect("rdeps"),
        thisDependsOn: findDirect("depends"),
        dependsOnThisTransitive: findTransitive("depends", false),
        thisDependsOnTransitive: findTransitive("rdeps", true)
    };
}
function colorAnd(c1, c2) {
    return c1 === null ? c2 : c1 === c2 ? c1 : undefined;
}
var Result = (function () {
    function Result() {
    }
    return Result;
})();
function ruleFilter(dat, query) {
    queryData = dat;
    var f = readQuery(query);
    var res = {};
    for (queryKey = 0; queryKey < dat.original.length; queryKey++) {
        queryVal = dat.original[queryKey];
        queryName = queryVal.name;
        queryGroup = null;
        queryBackColor = null;
        queryTextColor = null;
        if (f()) {
            if (queryGroup === null)
                queryGroup = queryName;
            if (!(queryGroup in res))
                res[queryGroup] = { items: [queryKey], text: queryTextColor, back: queryBackColor };
            else {
                var c = res[queryGroup];
                c.items.push(queryKey);
                c.text = colorAnd(c.text, queryTextColor);
                c.back = colorAnd(c.back, queryBackColor);
            }
        }
    }
    return res;
}
var ResultTable = (function () {
    function ResultTable() {
    }
    return ResultTable;
})();
function ruleTable(dat, query) {
    function bools(x, y) {
        return x === "" ? y : x === y ? x : "both";
    }
    var res = ruleFilter(dat, query);
    var ans = [];
    for (var s in res) {
        var xs = res[s].items;
        var time = 0;
        var leaf = "";
        var unchanged = "";
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
var ResultGraph = (function () {
    function ResultGraph() {
    }
    return ResultGraph;
})();
function ruleGraph(dat, query) {
    var res = ruleFilter(dat, query);
    var map = {};
    var direct = {};
    var ind = -1;
    for (var s in res) {
        ind++;
        var xs = res[s].items;
        for (var i = 0; i < xs.length; i++)
            direct[xs[i]] = ind;
    }
    function getDirect(key) {
        return key in direct ? [direct[key]] : [];
    }
    var indirect = {};
    function getIndirect(key) {
        if (key in indirect)
            return indirect[key];
        if (key in direct)
            return [];
        var ds = dat.original[key].depends;
        var res = [];
        for (var j = 0; j < ds.length; j++) {
            res.push(getIndirect(ds[j]));
            res.push(getDirect(ds[j]));
        }
        var res2 = concatNub(res);
        indirect[key] = res2;
        return res2;
    }
    var ans = [];
    for (var s in res) {
        var xs = res[s].items;
        var ds = [];
        var is = [];
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
function commandFilter(last, dat, query) {
    queryData = dat;
    var f = readQuery(query);
    var res = {};
    for (queryKey = 0; queryKey < dat.original.length; queryKey++) {
        queryVal = dat.original[queryKey];
        if (last && queryVal.built !== 0)
            continue;
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
                if (queryGroup === null)
                    queryGroup = ts[i].command;
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
var CommandTable = (function () {
    function CommandTable() {
    }
    return CommandTable;
})();
function commandTable(dat, query) {
    var res = commandFilter(false, dat, query);
    var ans = [];
    for (var s in res) {
        var xs = res[s].items;
        var time = 0;
        for (var _i = 0; _i < xs.length; _i++) {
            var t_2 = xs[_i];
            time += t_2.stop - t_2.start;
        }
        ans.push({ name: s, count: xs.length, text: res[s].text, back: res[s].back, time: time });
    }
    return ans;
}
function commandPlot(dat, query, buckets) {
    var end = dat.summary.maxTraceStopLast;
    var res = commandFilter(true, dat, query);
    var ans = {};
    for (var s in res) {
        var ts = res[s].items;
        var xs = [];
        for (var i = 0; i <= buckets; i++)
            xs.push(0);
        for (var _i = 0; _i < ts.length; _i++) {
            var t_3 = ts[_i];
            var start = t_3.start * buckets / end;
            var stop = t_3.stop * buckets / end;
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
function readQuery(query) {
    if (query === "")
        return function () { return true; };
    var f;
    try {
        f = (new Function("return " + query));
    }
    catch (e) {
        throw { user: true, name: "parse", query: query, message: e.toString() };
    }
    return function () {
        try {
            return f();
        }
        catch (e) {
            throw { user: true, name: "execution", query: query, message: e.toString() };
        }
    };
}
var queryData = {};
var queryKey = 0;
var queryVal = {};
var queryName = "";
var queryGroup = null;
var queryBackColor = null;
var queryTextColor = null;
function childOf(r) { return queryData.dependsOnThis(queryKey, r); }
function parentOf(r) { return queryData.thisDependsOn(queryKey, r); }
function ancestorOf(r) { return queryData.dependsOnThisTransitive(queryKey, r); }
function descendantOf(r) { return queryData.thisDependsOnTransitive(queryKey, r); }
function descendentOf(r) { return descendantOf(r); }
function group(x) {
    if (queryGroup === null)
        queryGroup = "";
    queryGroup += (queryGroup === "" ? "" : " ") + x;
    return true;
}
function backColor(c, b) {
    if (b === void 0) { b = true; }
    if (b)
        queryBackColor = c;
    return true;
}
function textColor(c, b) {
    if (b === void 0) { b = true; }
    if (b === undefined || b)
        queryTextColor = c;
    return true;
}
function rename(from, to) {
    if (to === void 0) { to = ""; }
    queryName = queryName.replace(from, to);
    return true;
}
function slowestRule() {
    return queryData.summary.maxExecutionName;
}
function leaf() {
    return queryVal.depends.length === 0;
}
function run(i) {
    if (i === undefined)
        return queryVal.built;
    else
        return queryVal.built === i;
}
function unchanged() {
    return queryVal.changed !== queryVal.built;
}
function named(r, groupName) {
    if (r === undefined)
        return queryName;
    var res = execRegExp(r, queryName);
    if (res === null) {
        if (groupName === undefined)
            return false;
        else {
            group(groupName);
            return true;
        }
    }
    if (res.length !== 1) {
        for (var i = 1; i < res.length; i++)
            group(res[i]);
    }
    return true;
}
function command(r, groupName) {
    var n = (queryVal.traces || []).length;
    if (r === undefined)
        return n === 0 ? "" : queryVal.traces[0].command;
    for (var _i = 0, _a = queryVal.traces; _i < _a.length; _i++) {
        var t_4 = _a[_i];
        var res = execRegExp(r, t_4.command);
        if (res === null)
            continue;
        if (res.length !== 1) {
            for (var j = 1; j < res.length; j++)
                group(res[j]);
        }
        return true;
    }
    if (groupName === undefined)
        return false;
    else {
        group(groupName);
        return true;
    }
}
"use strict";
function initProgress() {
    $(function () {
        $(".version").html("Generated by <a href='http://shakebuild.com'>Shake " + version + "</a>.");
        $("#output").html("");
        for (var _i = 0; _i < progress.length; _i++) {
            var x = progress[_i];
            var actual = [];
            var ideal = [];
            for (var t = 5; t < x.values.length; t++) {
                var y = x.values[t];
                actual.push([y.idealSecs, y.actualSecs]);
                ideal.push([y.idealSecs, y.idealSecs]);
            }
            var ys = [{ data: ideal, color: "gray" }, { label: x.name, data: actual, color: "red" }];
            var div = $("<div class='plot'>");
            $("#output").append(div);
            $.plot(div, ys, {
                xaxis: {
                    transform: function (v) { return -v; },
                    inverseTransform: function (v) { return -v; }
                }
            });
        }
    });
}
"use strict";
function t(a, b, c) { return { start: a, stop: b, command: c }; }
var dat1 = [{ name: "Functional", built: 0, changed: 3, depends: [], execution: 1, traces: [t(0, 1, "gen")] },
    { name: "Imperative", built: 0, changed: 0, depends: [], execution: 2, traces: [t(0, 1, "gen"), t(1, 2, "gen")] },
    { name: "HsSource", built: 3, changed: 3, depends: [], execution: 0 },
    { name: "Haskell", built: 3, changed: 3, depends: [0, 2], execution: 8, traces: [t(1, 8.9, "ghc")] },
    { name: "C", built: 0, changed: 0, depends: [1], execution: 15, traces: [t(2, 16.9, "gcc")] },
    { name: "Cpp", built: 0, changed: 0, depends: [1], execution: 10, traces: [t(2, 10, "gcc")] },
    { name: "Exe", built: 0, changed: 0, depends: [3, 4, 5], execution: 5, traces: [t(17, 22, "link")] }
];
function test() {
    function assert(b) {
        if (!b)
            throw "Assertion failed";
    }
    function assertEq(got, want) {
        if (want != got) {
            console.log("Wanted: " + want);
            console.log("Got: " + got);
            assert(false);
        }
    }
    function assertRegex(want, got) {
        if (!want.test(got)) {
            console.log("Wanted: " + want);
            console.log("Got: " + got);
            assert(false);
        }
    }
    var tab1 = prepare(dat1);
    var ssum1 = showSummary(tab1.summary);
    console.log(ssum1);
    var want = ["4 runs", "7 rules", "5 rebuilt", "7 traced", "6 in", "build time is 41.00s", "38.80s is traced",
        "longest rule takes 15.00s", "longest traced command takes 14.90s", "parallelism of 1.40", "22.00s"];
    assertRegex(new RegExp(want.join(".*")), ssum1.join(" "));
    var par1 = commandPlot(tab1, "group('x')", 10)['x'];
    console.log(par1);
    var pars1 = par1.items.map(function (i) { return Math.round(i * 10) / 10; });
    assert(listEq(pars1, [1.5, 2, 2, 2, 1.5, 1, 1, 1, 1, 1]));
    function chk(f, query, n) {
        var ans = f(tab1, query);
        console_table(ans);
        assertEq(ans.length, n);
    }
    chk(ruleTable, "", 7);
    chk(ruleTable, "leaf()", 3);
    chk(ruleTable, "named(/^(.)/)", 5);
    chk(commandTable, "", 4);
    chk(commandTable, "command(/g(.*)/)", 3);
    chk(ruleTable, "childOf('Imperative')", 2);
    return "passed";
}
function console_table(xs) {
    if ("table" in console)
        console["table"](xs);
    else if (xs.length === 0)
        console.log("No data");
    else {
        var widths = [];
        var cells = [];
        for (var i_1 = 0; i_1 <= xs.length; i_1++)
            cells.push([]);
        for (var s_1 in xs[0]) {
            var len = s_1.length;
            cells[0].push(s_1);
            for (var i = 0; i < xs.length; i++) {
                var ss = "" + xs[i][s_1];
                len = Math.max(len, ss.length);
                cells[i + 1].push(ss);
            }
            widths.push(len);
        }
        var s = "";
        for (var x = 0; x < cells.length; x++) {
            for (var y = 0; y < widths.length; y++)
                s += "|" + pad(widths[y], cells[x][y]);
            s += "|\n";
        }
        console.log(s);
    }
}
function pad(n, s) {
    var res = s;
    for (var i = s.length; i < n; i++)
        res += " ";
    return res;
}
"use strict";
var prepared = prepare(profile);
var currentTable = null;
var Report = (function () {
    function Report(mode_, query_) {
        if (mode_ === void 0) { mode_ = "summary"; }
        if (query_ === void 0) { query_ = ""; }
        this.sort = "time";
        this.sortRev = false;
        this.mode = mode_;
        this.query = query_;
    }
    return Report;
})();
var report = new Report(null, null);
function reportEq(r1, r2) {
    return r1.mode === r2.mode && r1.query === r2.query && r1.sort === r2.sort && r1.sortRev === r2.sortRev;
}
function reportToURL(r) {
    var def = new Report();
    return "?mode=" + r.mode +
        (r.query === def.query ? "" : "&query=" + encodeURI(r.query).replace(/\+/g, "%2B")) +
        ((!r.sortRev && r.sort === def.sort) ? "" :
            "&sort=" + (r.sortRev ? "!" : "") + r.sort);
}
function reportFromURL(s) {
    if (s === void 0) { s = window.location.search; }
    var res = new Report();
    var params = uriQueryParameters(s);
    if ("mode" in params)
        res.mode = params["mode"];
    if ("query" in params)
        res.query = params["query"];
    if ("sort" in params) {
        var sort = params["sort"];
        res.sortRev = sort.substr(0, 1) == "!";
        res.sort = sort.substr(res.sortRev ? 1 : 0);
    }
    return res;
}
function reportFromUser() {
    return new Report($("#mode").val(), $("#query").val());
}
function setReport(set, replace, run) {
    var report2 = set(recordCopy(report));
    $("#mode").val(report2.mode);
    $("#query").val(report2.query);
    $("#run").enable(false).attr("title", "The current query is displayed");
    if (reportEq(report, report2))
        return;
    report = report2;
    if (window.history) {
        var title = report.mode + (report.query === "" ? "" : ": " + report.query);
        var url = reportToURL(report);
        try {
            if (replace)
                window.history.replaceState(report, title, url);
            else
                window.history.pushState(report, title, url);
        }
        catch (e) {
        }
    }
    $("#link").attr("href", reportToURL(report));
    if (run)
        runReport();
}
var rightAlign = { count: null, time: null, cost: null, run: null, leaf: null, unchanged: null };
var twoColumns = { cost: null, time: null };
var defaultRevSort = { run: null, name: null };
function tableSort(x) {
    if (report.sort === x)
        setReport(function (r) { r.sortRev = !r.sortRev; return r; }, true, false);
    else
        setReport(function (r) { r.sort = x; r.sortRev = x in defaultRevSort; return r; }, true, false);
    showTable(currentTable);
}
function showTable(xs) {
    currentTable = xs;
    if (xs.length === 0) {
        $("#output").html("No data found");
        return;
    }
    if (!(report.sort in xs[0]))
        setReport(function (r) { return new Report(r.mode, r.query); }, true, false);
    xs.sort(function (a, b) { return (report.sortRev ? -1 : 1) * (b[report.sort] > a[report.sort] ? 1 : -1); });
    var res = "<table class='data'><tr class='header'>";
    for (var s in xs[0]) {
        if (s === "back" || s === "text")
            continue;
        res += s in twoColumns ? "<td colspan='2' style='text-align:center;'" :
            s in rightAlign ? "<td style='text-align:right;'" :
                "<td";
        res += " onclick=\"tableSort('" + s + "')\">" + s;
        if (s === report.sort)
            res += " <span class='sort'>" + (report.sortRev ? "&#9650;" : "&#9660;") + "</span>";
        res += "</td>";
    }
    res += "</tr>";
    for (var _i = 0; _i < xs.length; _i++) {
        var x = xs[_i];
        res += "<tr";
        if (x["back"])
            res += " style='background-color:" + x["back"] + ";'";
        if (x["text"])
            res += " style='color:" + x["text"] + ";'";
        res += ">";
        for (var s in xs[0]) {
            if (s === "text" || s === "back")
                continue;
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
var currentPlot = null;
function showPlot(series, options) {
    var $output = $("#output");
    var width = $output.width();
    var height = $output.height();
    if (series === null && options === null) {
        if (width === currentPlot.width && height === currentPlot.height)
            return;
        series = currentPlot.series;
        options = currentPlot.options;
    }
    currentPlot = { series: series, options: options, width: width, height: height };
    var div = $("<div>").width(width - 20).height(height - 10);
    $("#output").html("").append(div);
    $.plot(div, series, options);
}
window.onresize = function () {
    if (currentPlot !== null)
        showPlot(null, null);
};
function runReport() {
    currentTable = null;
    currentPlot = null;
    try {
        switch (report.mode) {
            case "summary":
                var res = showSummary(prepared.summary);
                var s_2 = $("#welcome").html();
                s_2 += "<ul>";
                for (var i = 0; i < res.length; i++)
                    s_2 += "<li>" + res[i] + "</li>";
                s_2 += "</ul>";
                s_2 += "<p class='version'>Generated by <a href='http://shakebuild.com'>Shake " + version + "</a>.</p>";
                $("#output").html(s_2);
                break;
            case "cmd-plot":
                {
                    var xs = commandPlot(prepared, report.query, 100);
                    var ys = [];
                    for (var s_3 in xs) {
                        var x = xs[s_3].items;
                        var data = [];
                        for (var j = 0; j < x.length; j++)
                            data.push([j, x[j]]);
                        ys.push({ label: s_3, data: data, color: xs[s_3].back, avg: sum(x) / x.length });
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
                }
                break;
            case "cmd-table":
                showTable(commandTable(prepared, report.query));
                break;
            case "rule-table":
                showTable(ruleTable(prepared, report.query));
                break;
            case "rule-graph":
                {
                    var xs = ruleGraph(prepared, report.query);
                    if (xs.length > 250)
                        $("#output").html("Viewing a graph with > 250 nodes is not supported, and you have " + xs.length + " nodes. Try grouping more aggressively");
                    else if (typeof Viz === 'undefined')
                        $("#output").html("Profile reports do not seem to have been built with GraphViz support, this feature is unavailable.");
                    else {
                        var res_1 = "digraph \"\"{";
                        res_1 += "graph[nodesep=0.15,ranksep=0.3];";
                        res_1 += "node[fontname=\"sans-serif\",fontsize=9,penwidth=0.5,height=0,width=0];";
                        res_1 += "edge[penwidth=0.5,arrowsize=0.5];";
                        for (var i = 0; i < xs.length; i++) {
                            res_1 += "a" + i + "[label=\"" + xs[i].name.split("\\").join("\\\\").split("\"").join("\\\"") + "\"";
                            if (xs[i].back)
                                res_1 += ",style=filled,color=\"" + xs[i].back + "\"";
                            if (xs[i].text)
                                res_1 += ",fontcolor=\"" + xs[i].text + "\"";
                            res_1 += "];";
                            var parents = xs[i].parents;
                            for (var j = 0; j < parents.length; j++)
                                res_1 += "a" + i + "->a" + parents[j] + ";";
                            var ancestors = xs[i].ancestors;
                            for (var j = 0; j < ancestors.length; j++)
                                res_1 += "a" + i + "->a" + ancestors[j] + "[style=dashed];";
                        }
                        res_1 += "}";
                        $("#output").html(Viz(res_1, "svg"));
                    }
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
    catch (e) {
        if (!(e && e.user))
            throw e;
        $("#output").html($("#error").html());
        for (var s in e)
            $("#output ." + s).text(e[s]);
    }
}
function example(mode, query) {
    setReport(function (_) { return new Report(mode, query); }, false, true);
    return false;
}
function initProfile() {
    $(function () {
        setReport(function (_) { return reportFromURL(); }, true, true);
        $("#mode,#query").bind("input change", function () {
            var mode = $("#mode").val();
            var query = $("#query").val();
            var enable = mode !== report.mode || query !== report.query;
            $("#run").enable(enable).attr("title", enable ? "" : "The current query is displayed");
            $("#link").attr("href", reportToURL(reportFromUser()));
        });
        $("#run").click(function () {
            setReport(function (_) { return reportFromUser(); }, false, true);
        });
        $("#query").keypress(function (e) {
            if (e.which == 13)
                $("#run").click();
        });
        window.onpopstate = function (e) {
            setReport(function (_) { return reportFromUser(); }, true, true);
        };
        $("a.example").each(function () {
            var mode = $(this).attr("data-mode");
            var query = $(this).attr("data-query");
            if (query === undefined)
                query = $(this).text();
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
"use strict";
jQuery.fn.enable = function (x) {
    return this.each(function () {
        if (x)
            $(this).removeAttr('disabled');
        else
            $(this).attr('disabled', 'disabled');
    });
};
function uriQueryParameters(s) {
    var params = {};
    var a = /\+/g;
    var r = /([^&=]+)=?([^&]*)/g;
    var d = function (s) { return decodeURIComponent(s.replace(a, " ")); };
    var q = s.substring(1);
    while (true) {
        var e = r.exec(q);
        if (!e)
            break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
}
;
function showTime(x) {
    function digits(x) { var s = String(x); return s.length === 1 ? "0" + s : s; }
    if (x >= 3600) {
        x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    }
    else if (x >= 60) {
        x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    }
    else
        return x.toFixed(2) + "s";
}
function showPerc(x) {
    return (x * 100).toFixed(2) + "%";
}
function plural(n, not1, is1) {
    if (not1 === void 0) { not1 = "s"; }
    if (is1 === void 0) { is1 = ""; }
    return n === 1 ? is1 : not1;
}
function sum(xs) {
    var res = 0;
    for (var _i = 0; _i < xs.length; _i++) {
        var x = xs[_i];
        res += x;
    }
    return res;
}
function testRegExp(r, s) {
    if (typeof r === "string")
        return s.indexOf(r) !== -1;
    else
        return r.test(s);
}
function execRegExp(r, s) {
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}
function listEq(xs, ys) {
    if (xs.length !== ys.length)
        return false;
    for (var i = 0; i < xs.length; i++) {
        if (xs[i] !== ys[i])
            return false;
    }
    return true;
}
function cache(key, op) {
    var store = {};
    return function (k) {
        var s = key(k);
        if (!(s in store))
            store[s] = op(k);
        return store[s];
    };
}
function mapEq(xs, ys) {
    function f(a, b) {
        for (var s in a) {
            if (a[s] !== b[s])
                return false;
        }
        return true;
    }
    return f(xs, ys) && f(ys, xs);
}
function recordCopy(xs) {
    return mapCopy(xs);
}
function mapCopy(xs) {
    var res = {};
    for (var s in xs)
        res[s] = xs[s];
    return res;
}
function mapUnion(xs, ys) {
    var res = mapCopy(ys);
    for (var s in xs)
        res[s] = xs[s];
    return res;
}
function concatNub(xss) {
    var res = [];
    var seen = {};
    for (var _i = 0; _i < xss.length; _i++) {
        var xs = xss[_i];
        for (var _a = 0; _a < xs.length; _a++) {
            var x = xs[_a];
            var v = x;
            if (!(v in seen)) {
                seen[v] = null;
                res.push(x);
            }
        }
    }
    return res;
}
