"use strict";
var __assign = (this && this.__assign) || function () {
    __assign = Object.assign || function(t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
            s = arguments[i];
            for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
                t[p] = s[p];
        }
        return t;
    };
    return __assign.apply(this, arguments);
};
function unrawTrace(x) {
    return {
        command: x[0],
        start: x[1],
        stop: x[2]
    };
}
function unrawProfile(x) {
    return {
        name: x[0],
        execution: x[1],
        built: x[2],
        changed: x[3],
        depends: x.length > 4 ? x[4] : [],
        traces: x.length > 5 ? x[5].map(unrawTrace) : []
    };
}
function profileRoot() {
    var _a = createSearch(profile), s = _a[0], search = _a[1];
    var t = createTabs([["Summary", function () { return reportSummary(profile, search); }], ["Summary", function () { return React.createElement("div", null, "Here"); }]]);
    return React.createElement("div", { style: "background-color:#e8e8e8;" },
        s,
        React.createElement("br", null),
        t);
}
function createTabs(xs) {
    var bodies = xs.map(function (x) { return lazy(x[1]); });
    var body = React.createElement("div", null);
    var lbls = [];
    var f = function (i) { return function () {
        $(body).empty().append(bodies[i]());
        lbls.map(function (x, j) { return $(x).toggleClass("active", i === j); });
    }; };
    lbls = xs.map(function (x, i) { return React.createElement("a", { onclick: f(i) }, x[0]); });
    f(0)();
    return React.createElement("div", null,
        React.createElement("div", { class: "tabstrip" }, lbls),
        React.createElement("div", { style: "background-color:white;padding-top:5px;" },
            React.createElement("div", null, body)));
}
function createSearch(profile) {
    var search = {};
    for (var i = 0; i < profile.length; i++)
        search[profile[i].name] = [i];
    var caption = React.createElement("div", null,
        "Found ",
        profile.length,
        " entries, not filtered or grouped.");
    var dropdown = React.createElement("div", { style: "border:1px solid gray;display:none;position:absolute;" },
        "Add stuff to the inner here",
        React.createElement("br", null),
        "And more stuff");
    var show_inner = function () { return $(dropdown).toggle(); };
    var body = (React.createElement("table", { style: "width:100%;" },
        React.createElement("tr", null,
            React.createElement("td", { width: "100%" },
                React.createElement("input", { id: "search", type: "text", value: "", placeholder: "Filter and group", style: "width: 100%; font-size: 16px; border-radius: 8px; padding: 5px; border-width: 2px; border-color: #999;" })),
            React.createElement("td", null,
                React.createElement("button", { style: "white-space:nowrap;padding-top:5px;padding-bottom:5px;", onclick: show_inner },
                    React.createElement("b", null, "+"),
                    " Filter and Group \u25BC"),
                dropdown)),
        React.createElement("tr", null,
            React.createElement("td", null, caption))));
    return [body, new Prop(search)];
}
function fullSearch() {
    var res = {};
    for (var i in profile)
        res[profile[i].name] = [i];
    return res;
}
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
/////////////////////////////////////////////////////////////////////
// ENVIRONMENT
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
// These are global variables mutated/queried by query execution
var queryData = {};
var queryKey = 0;
var queryVal = {};
var queryName = "";
var queryGroup = null;
var queryBackColor = null;
var queryTextColor = null;
function rs_key(k) {
    return typeof k === "string" ? "s" + k : "r" + k.source;
}
// before =
var before_Cache = cache(rs_key, function (k) {
    var res = {};
    var match = {};
    // go in reverse because its topo-sorted
    for (var i = profile.length - 1; i >= 0; i--) {
        if (testRegExp(k, profile[i].name))
            match[i] = null;
        if (profile[i].depends.some(function (j) { return j in match; }))
            res[i] = null;
    }
    return res;
});
function before(r) {
    return true;
}
function after(r) {
    return true;
}
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
var slowestRule_Cache = lazy(function () {
    var time = -1;
    var name = "";
    for (var _i = 0, profile_1 = profile; _i < profile_1.length; _i++) {
        var p = profile_1[_i];
        if (p[1] <= time)
            continue;
        name = p[0];
        time = p[1];
    }
    return name;
});
function slowestRule() {
    return slowestRule_Cache();
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
        var t_1 = _a[_i];
        var res = execRegExp(r, t_1.command);
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
//////////////////////////////////////////////////////////////////////
// SUMMARY
var Summary = /** @class */ (function () {
    function Summary() {
        this.sumExecution = 0; // build time in total
        this.countTraceLast = 0; // traced commands run
        this.maxTraceStopLast = 0; // time the last traced command stopped
    }
    return Summary;
}());
function summary(dat) {
    var res = new Summary();
    // Fold over dat to produce the summary
    for (var _i = 0, dat_1 = dat; _i < dat_1.length; _i++) {
        var e = dat_1[_i];
        var isLast = e.built === 0;
        res.sumExecution += e.execution;
        var traces = e.traces;
        if (!traces)
            continue;
        for (var _a = 0, traces_1 = traces; _a < traces_1.length; _a++) {
            var t_2 = traces_1[_a];
            var time = t_2.stop - t_2.start;
            res.countTraceLast += isLast ? 1 : 0;
            res.maxTraceStopLast = Math.max(res.maxTraceStopLast, isLast ? t_2.stop : 0);
        }
    }
    return res;
}
var Prepare = /** @class */ (function () {
    function Prepare() {
    }
    return Prepare;
}());
// Mutate the input data, adding in rdeps, being the 1-level reverse dependencies
function addRdeps(dat) {
    // find the reverse dependencies
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
// Given an array of indices, calculate the cost to rebuild if all of them change
// You must call addRdeps and addCost first
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
        for (var _i = 0, xs_1 = xs; _i < xs_1.length; _i++) {
            var x = xs_1[_i];
            f(x);
        }
    }
    return tot;
}
// Mutate the dat data, adding in cost, being the cost to rebuild if this item changes
function addCost(dat) {
    var res = dat;
    for (var i = 0; i < dat.length; i++) {
        // This call is type safe because calcRebuildCosts only ever looks at earlier items,
        // and those earlier items all have their cost filled in
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
            for (var _i = 0, dat_2 = dat; _i < dat_2.length; _i++) {
                var e = dat_2[_i];
                if (testRegExp(r, e.name)) {
                    var deps = (e)[key];
                    for (var _a = 0, deps_1 = deps; _a < deps_1.length; _a++) {
                        var j = deps_1[_a];
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
                    for (var _i = 0, deps_2 = deps; _i < deps_2.length; _i++) {
                        var k = deps_2[_i];
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
/////////////////////////////////////////////////////////////////////
// RULES
function colorAnd(c1, c2) {
    return c1 === null ? c2 : c1 === c2 ? c1 : undefined;
}
var Result = /** @class */ (function () {
    function Result() {
    }
    return Result;
}());
var ResultTable = /** @class */ (function () {
    function ResultTable() {
    }
    return ResultTable;
}());
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
var ResultGraph = /** @class */ (function () {
    function ResultGraph() {
    }
    return ResultGraph;
}());
function ruleGraph(dat, query) {
    var res = ruleFilter(dat, query);
    var map = {}; // which nodes a node lives at
    // loop through each value in res, putting it into map (these are parents)
    // for any not present, descend through the dat.original list, if you aren't included, add, if you are included, skip
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
/////////////////////////////////////////////////////////////////////
// COMMANDS
function commandFilter(last, dat, query) {
    queryData = dat;
    var f = readQuery(query);
    var res = {};
    for (queryKey = 0; queryKey < dat.original.length; queryKey++) {
        queryVal = dat.original[queryKey];
        if (last && queryVal.built !== 0)
            continue;
        var val = __assign({}, queryVal);
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
var CommandTable = /** @class */ (function () {
    function CommandTable() {
    }
    return CommandTable;
}());
function commandTable(dat, query) {
    var res = commandFilter(false, dat, query);
    var ans = [];
    for (var s in res) {
        var xs = res[s].items;
        var time = 0;
        for (var _i = 0, xs_2 = xs; _i < xs_2.length; _i++) {
            var t_3 = xs_2[_i];
            time += t_3.stop - t_3.start;
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
            xs.push(0); // fill with 1 more element, but the last bucket will always be 0
        for (var _i = 0, ts_1 = ts; _i < ts_1.length; _i++) {
            var t_4 = ts_1[_i];
            var start = t_4.start * buckets / end;
            var stop = t_4.stop * buckets / end;
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
function initProgress() {
    $(function () {
        $(".version").html("Generated by <a href='https://shakebuild.com'>Shake " + version + "</a>.");
        $("#output").html("");
        for (var _i = 0, progress_1 = progress; _i < progress_1.length; _i++) {
            var x = progress_1[_i];
            var actual = [];
            var ideal = [];
            // Start at t = 5 seconds, since the early progress jumps a lot
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
function t(a, b, c) { return { start: a, stop: b, command: c }; }
var raw1 = 
// Haskell depends on Functional, C and Cpp depend on Imperative
// Exe depends on Haskell/C/Cpp
[["Functional", 1, 0, 3, [], [["gen", 0, 1]]],
    ["Imperative", 2, 0, 0, [], [["gen", 0, 1], ["gen", 1, 2]]],
    ["HsSource", 0, 3, 3, [], []],
    ["Haskell", 8, 3, 3, [0, 2], [["ghc", 1, 8.9]]],
    ["C", 15, 0, 0, [1], [["gcc", 2, 16.9]]],
    ["Cpp", 10, 0, 0, [1], [["gcc", 2, 10]]],
    ["Exe", 5, 0, 0, [3, 4, 5], [["link", 17, 22]]]
];
var dat1 = raw1.map(unrawProfile);
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
    profile = dat1;
    var ssum1 = reportSummary(profile, createSearch(profile)[1]).innerText;
    console.log(ssum1);
    var want = ["4 runs", "7 rules", "5 rebuilt", "7 traced", "6 in", "build time is 41.00s", "38.80s is traced",
        "longest rule takes 15.00s", "longest traced command takes 14.90s", "parallelism of 1.40", "22.00s"];
    assertRegex(new RegExp(want.join(".*")), ssum1);
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
    // Could call console.table, but that doesn't print anything through 'node'
    if (xs.length === 0)
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
var profile = profileRaw.map(unrawProfile);
var prepared = prepare(profile);
var currentTable = null;
/////////////////////////////////////////////////////////////////////
// REPORT
var Report = /** @class */ (function () {
    function Report(mode_, query_) {
        if (mode_ === void 0) { mode_ = "summary"; }
        if (query_ === void 0) { query_ = ""; }
        this.sort = "time";
        this.sortRev = false;
        this.mode = mode_;
        this.query = query_;
    }
    return Report;
}());
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
    var report2 = set(__assign({}, report));
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
            // Chrome disallows replaceState from origin null
        }
    }
    $("#link").attr("href", reportToURL(report));
    if (run)
        runReport();
}
/////////////////////////////////////////////////////////////////////
// TABLE SHOWING
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
    for (var _i = 0, xs_3 = xs; _i < xs_3.length; _i++) {
        var x = xs_3[_i];
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
    // Fudge factors to get it displaying nicely, seems Flot goes outside its bounds
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
            case "prototype":
                $("#output").empty().append(profileRoot());
                break;
            case "summary":
                $("#output").empty().append(reportSummary(profile, createSearch(profile)[1]));
                break;
            case "cmd-plot":
                {
                    var xs = commandPlot(prepared, report.query, 100);
                    var ys = [];
                    for (var s_2 in xs) {
                        var x = xs[s_2].items;
                        var data = [];
                        for (var j = 0; j < x.length; j++)
                            data.push([j, x[j]]);
                        ys.push({ label: s_2, /* values:x, */ data: data, color: xs[s_2].back, avg: sum(x) / x.length });
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
                        var res = "digraph \"\"{";
                        res += "graph[nodesep=0.15,ranksep=0.3];";
                        res += "node[fontname=\"sans-serif\",fontsize=9,penwidth=0.5,height=0,width=0];";
                        res += "edge[penwidth=0.5,arrowsize=0.5];";
                        for (var i = 0; i < xs.length; i++) {
                            res += "a" + i + "[label=\"" + xs[i].name.split("\\").join("\\\\").split("\"").join("\\\"") + "\"";
                            if (xs[i].back)
                                res += ",style=filled,color=\"" + xs[i].back + "\"";
                            if (xs[i].text)
                                res += ",fontcolor=\"" + xs[i].text + "\"";
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
/////////////////////////////////////////////////////////////////////
// STATE NAVIGATION
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
            var href = "https://hackage.haskell.org/packages/archive/shake/latest/doc/html/Development-Shake.html#v:" +
                $(this).text().replace("'", "-39-");
            $(this).attr("href", href).attr("target", "_blank");
        });
    });
}
jQuery.fn.enable = function (x) {
    // Set the values to enabled/disabled
    return this.each(function () {
        if (x)
            $(this).removeAttr("disabled");
        else
            $(this).attr("disabled", "disabled");
    });
};
/////////////////////////////////////////////////////////////////////
// BROWSER HELPER METHODS
// Given "?foo=bar&baz=1" returns {foo:"bar",baz:"1"}
function uriQueryParameters(s) {
    // From https://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    var params = {};
    var a = /\+/g; // Regex for replacing addition symbol with a space
    var r = /([^&=]+)=?([^&]*)/g;
    var d = function (x) { return decodeURIComponent(x.replace(a, " ")); };
    var q = s.substring(1);
    while (true) {
        var e = r.exec(q);
        if (!e)
            break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
}
/////////////////////////////////////////////////////////////////////
// STRING FORMATTING
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
/////////////////////////////////////////////////////////////////////
// MISC
function sum(xs) {
    var res = 0;
    for (var _i = 0, xs_4 = xs; _i < xs_4.length; _i++) {
        var x = xs_4[_i];
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
function lazy(thunk) {
    var store = null;
    var done = false;
    return function () {
        if (!done) {
            store = thunk();
            done = true;
        }
        return store;
    };
}
function concatNub(xss) {
    var res = [];
    var seen = {};
    for (var _i = 0, xss_1 = xss; _i < xss_1.length; _i++) {
        var xs = xss_1[_i];
        for (var _a = 0, xs_5 = xs; _a < xs_5.length; _a++) {
            var x = xs_5[_a];
            var v = x;
            if (!(v in seen)) {
                seen[v] = null;
                res.push(x);
            }
        }
    }
    return res;
}
// Use JSX with el instead of React.createElement
// Originally from https://gist.github.com/sergiodxa/a493c98b7884128081bb9a281952ef33
// our element factory
function createElement(type, props) {
    var _children = [];
    for (var _i = 2; _i < arguments.length; _i++) {
        _children[_i - 2] = arguments[_i];
    }
    // if _children is an array of array take the first value, else take the full array
    var children = Array.isArray(_children[0]) ? _children[0] : _children;
    var element = document.createElement(type);
    for (var name_1 in props || {}) {
        if (name_1.substr(0, 2) === "on")
            element.addEventListener(name_1.substr(2), props[name_1]);
        else
            element.setAttribute(name_1, props[name_1]);
    }
    for (var _a = 0, children_1 = children; _a < children_1.length; _a++) {
        var child = children_1[_a];
        var c = typeof child === "object" ? child : document.createTextNode(child.toString());
        element.appendChild(c);
    }
    return element;
}
// How .tsx gets desugared
var React = { createElement: createElement };
// Stuff that Shake generates and injects in
/////////////////////////////////////////////////////////////////////
// BASIC UI TOOLKIT
var Prop = /** @class */ (function () {
    function Prop(val) {
        this.val = val;
    }
    Prop.prototype.get = function () { return this.val; };
    Prop.prototype.set = function (val) {
        this.val = val;
        this.callback(val);
    };
    Prop.prototype.event = function (next) {
        var old = this.callback;
        this.callback = function (val) { old(val); next(val); };
    };
    return Prop;
}());
function reportSummary(profile, search) {
    var count = 0; // number of rules run
    var countLast = 0; // number of rules run in the last run
    var highestRun = 0; // highest run you have seen (add 1 to get the count of runs)
    var sumExecution = 0; // build time in total
    var maxExecution = 0; // longest build rule
    var maxExecutionName = ""; // longest build rule
    var countTrace = 0;
    var countTraceLast = 0; // traced commands run
    var sumTrace = 0;
    var sumTraceLast = 0; // time running traced commands
    var maxTrace = 0; // longest traced command
    var maxTraceName = ""; // longest trace command
    var maxTraceStopLast = 0; // time the last traced command stopped
    var s = search.get();
    for (var k in s) {
        for (var _i = 0, _a = s[k]; _i < _a.length; _i++) {
            var i = _a[_i];
            var e = profile[i];
            var isLast = e.built === 0;
            count++;
            countLast += isLast ? 1 : 0;
            sumExecution += e.execution;
            maxExecution = Math.max(maxExecution, e.execution);
            if (maxExecution === e.execution)
                maxExecutionName = e.name;
            highestRun = Math.max(highestRun, e.changed); // changed is always greater or equal to built
            for (var _b = 0, _c = e.traces || []; _b < _c.length; _b++) {
                var t_5 = _c[_b];
                var time = t_5.stop - t_5.start;
                countTrace += 1;
                countTraceLast += isLast ? 1 : 0;
                sumTrace += time;
                sumTraceLast += isLast ? time : 0;
                maxTrace = Math.max(maxTrace, time);
                if (maxTrace === time)
                    maxTraceName = t_5.command;
                maxTraceStopLast = Math.max(maxTraceStopLast, isLast ? t_5.stop : 0);
            }
        }
    }
    var lines = ["This database has tracked " + (highestRun + 1) + " run" + plural(highestRun + 1) + ".",
        "There are " + count + " rules (" + countLast + " rebuilt in the last run).",
        "Building required " + countTrace + " traced commands (" + countTraceLast + " in the last run).",
        "The total (unparallelised) build time is " + showTime(sumExecution) + " of which " + showTime(sumTrace) + " is traced commands.",
        "The longest rule takes " + showTime(maxExecution) + " (" + maxExecutionName + ") and the longest traced command takes " + showTime(maxTrace) + " (" + maxTraceName + ").",
        "Last run gave an average parallelism of " + (maxTraceStopLast === 0 ? 0 : sumTraceLast / maxTraceStopLast).toFixed(2) + " times over " + showTime(maxTraceStopLast) + "."
    ];
    return React.createElement("div", null,
        React.createElement("ul", null, lines.map(function (s) { return React.createElement("li", null, s); })),
        React.createElement("p", { class: "version" },
            "Generated by ",
            React.createElement("a", { href: "https://shakebuild.com" },
                "Shake ",
                version),
            "."));
}
