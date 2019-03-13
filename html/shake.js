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
function bindPlot(element, data, options) {
    var redraw = function () {
        if ($(element).is(":visible"))
            $.plot($(element), data.get(), options);
    };
    window.setTimeout(redraw, 1);
    $(window).on("resize", redraw);
    data.event(redraw);
}
function varLink(name) {
    return React.createElement("a", { href: "https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:" + name },
        React.createElement("tt", null, name));
}
function newTable(columns, data, sortColumn, sortDescend) {
    var f = function (x) { return ({ name: x.field, label: x.label, width: x.width, cellClasses: x.alignRight ? "right" : "" }); };
    var formatters = {};
    for (var _i = 0, columns_1 = columns; _i < columns_1.length; _i++) {
        var c = columns_1[_i];
        formatters[c.field] = c.show || (function (x) { return x; });
    }
    var table = new DGTable({
        adjustColumnWidthForSortArrow: false,
        cellFormatter: function (val, colname) { return formatters[colname](val); },
        columns: columns.map(f),
        width: DGTable.Width.SCROLL
    });
    $(table.el).css("height", "100%");
    window.setTimeout(function () {
        table.render();
        table.tableHeightChanged();
        if (sortColumn)
            table.sort(sortColumn, sortDescend);
        table.setRows(data.get(), true);
    }, 1);
    data.event(function (xs) {
        table.setRows(xs, true);
        table.render();
    });
    $(window).on("resize", function () {
        if ($(table.el).is(":visible"))
            table.tableHeightChanged();
    });
    return React.createElement("div", { style: "height:100%;width:100%;" }, table.el);
}
// These are global variables mutated/queried by query execution
var environmentAll; // All the profiles
var environmentThis; // The specific profile under test
var environmentGroup; // The group produced as a result
function group(x) {
    environmentGroup.push(x);
    return true;
}
function leaf() {
    return environmentThis.depends.length === 0;
}
function run(i) {
    if (i === undefined)
        return environmentThis.built;
    else
        return environmentThis.built === i;
}
function changed() {
    return environmentThis.changed === environmentThis.built;
}
function unchanged() {
    return !unchanged();
}
function named(r, groupName) {
    if (r === undefined)
        return environmentThis.name;
    var res = execRegExp(r, environmentThis.name);
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
    var n = (environmentThis.traces || []).length;
    if (r === undefined)
        return n === 0 ? "" : environmentThis.traces[0].command;
    for (var _i = 0, _a = environmentThis.traces; _i < _a.length; _i++) {
        var t = _a[_i];
        var res = execRegExp(r, t.command);
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
function profileLoaded(profileRaw) {
    $(document.body).empty().append(profileRoot(unraw(profileRaw)));
}
function unraw(xs) {
    var ans = xs.map(function (x, i) { return ({
        index: i,
        name: x[0],
        execution: x[1],
        built: x[2],
        changed: x[3],
        depends: x.length > 4 ? x[4] : [],
        rdepends: [],
        traces: x.length > 5 ? x[5].map(function (y) { return ({ command: y[0], start: y[1], stop: y[2] }); }) : []
    }); });
    for (var _i = 0, ans_1 = ans; _i < ans_1.length; _i++) {
        var p = ans_1[_i];
        for (var _a = 0, _b = p.depends; _a < _b.length; _a++) {
            var d = _b[_a];
            ans[d].rdepends.push(p.index);
        }
    }
    return ans;
}
function profileRoot(profile) {
    var _a = createSearch(profile), s = _a[0], search = _a[1];
    var t = createTabs([["Summary", function () { return reportSummary(profile); }],
        ["Command plot", function () { return reportCmdPlot(profile); }],
        ["Commands", function () { return reportCmdTable(profile, search); }],
        ["Rules", function () { return reportRuleTable(profile, search); }],
        ["Parallelizability", function () { return reportParallelism(profile); }],
        ["Why rebuild", function () { return reportRebuild(profile, search); }]
    ]);
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { style: "padding-top: 8px; padding-bottom: 8px;" },
                React.createElement("a", { href: "https://shakebuild.com/", style: "font-size: 20px; text-decoration: none; color: #3131a7; font-weight: bold;" }, "Shake profile report"),
                React.createElement("span", { style: "color:gray;white-space:pre;" },
                    "   - generated at ",
                    generated,
                    " by Shake v",
                    version))),
        React.createElement("tr", null,
            React.createElement("td", null, s)),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%" }, t)));
}
function createTabs(xs) {
    var bodies = xs.map(function (x) {
        var el = React.createElement("div", { style: "padding:5px;width:100%;height:100%;min-width:150px;min-height:150px;overflow:auto;display:none;" });
        var upd = lazy(function () { return $(el).append(x[1]()); });
        return pair(el, upd);
    });
    var lbls = [];
    var f = function (i) { return function () {
        bodies[i][1]();
        lbls.map(function (x, j) { return $(x).toggleClass("active", i === j); });
        bodies.map(function (x, j) { return $(x[0]).toggle(i === j); });
        $(window).trigger("resize");
    }; };
    lbls = xs.map(function (x, i) { return React.createElement("a", { onclick: f(i) }, x[0]); });
    f(0)();
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", null,
                React.createElement("table", { width: "100%", style: "border-spacing:0px;" },
                    React.createElement("tr", { class: "tabstrip" },
                        React.createElement("td", { width: "20", class: "bottom" }, "\u00A0"),
                        React.createElement("td", { style: "padding:0px;" }, lbls),
                        React.createElement("td", { width: "100%", class: "bottom" }, "\u00A0"))))),
        React.createElement("tr", { height: "100%" },
            React.createElement("td", { style: "background-color:white;" }, bodies.map(fst))));
}
// A mapping from names (rule names or those matched from rule parts)
// to the indicies in profiles.
var Search = /** @class */ (function () {
    function Search(profile, mapping) {
        this.profile = profile;
        if (mapping !== undefined)
            this.mapping = mapping;
        else {
            this.mapping = {};
            for (var _i = 0, profile_1 = profile; _i < profile_1.length; _i++) {
                var p = profile_1[_i];
                this.mapping[p.name] = [p.index];
            }
        }
    }
    Search.prototype.forEachProfiles = function (f) {
        var _this = this;
        for (var s in this.mapping)
            f(this.mapping[s].map(function (i) { return _this.profile[i]; }), s);
    };
    Search.prototype.forEachProfile = function (f) {
        this.forEachProfiles(function (ps, group) { return ps.forEach(function (p) { return f(p, group); }); });
    };
    Search.prototype.mapProfiles = function (f) {
        var res = [];
        this.forEachProfiles(function (ps, group) { return res.push(f(ps, group)); });
        return res;
    };
    Search.prototype.mapProfile = function (f) {
        var res = [];
        this.forEachProfile(function (p, group) { return res.push(f(p, group)); });
        return res;
    };
    return Search;
}());
function createSearch(profile) {
    var caption = React.createElement("div", null,
        "Found ",
        profile.length,
        " entries, not filtered or grouped.");
    var input = React.createElement("input", { id: "search", type: "text", value: "", placeholder: "Filter and group", style: "width: 100%; font-size: 16px; border-radius: 8px; padding: 5px 10px; border: 2px solid #999;" });
    var res = new Prop(new Search(profile));
    $(input).on("change keyup paste", function () {
        var s = $(input).val();
        if (s === "") {
            res.set(new Search(profile));
            $(caption).text("Found " + profile.length + " entries, not filtered or grouped.");
        }
        else if (s.indexOf("(") === -1) {
            var mapping = {};
            var found = 0;
            for (var _i = 0, profile_2 = profile; _i < profile_2.length; _i++) {
                var p = profile_2[_i];
                if (p.name.indexOf(s) !== -1) {
                    found++;
                    mapping[p.name] = [p.index];
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Substring filtered to " + found + " / " + profile.length + " entries, not grouped.");
        }
        else {
            var f = void 0;
            try {
                f = new Function("return " + s);
            }
            catch (e) {
                $(caption).text("Error compiling function, " + e);
                return;
            }
            var mapping = {};
            var groups = 0;
            var found = 0;
            environmentAll = profile;
            for (var _a = 0, profile_3 = profile; _a < profile_3.length; _a++) {
                var p = profile_3[_a];
                environmentThis = p;
                environmentGroup = [];
                var bool = void 0;
                try {
                    bool = f();
                }
                catch (e) {
                    $(caption).text("Error running function, " + e);
                    return;
                }
                if (bool) {
                    found++;
                    var name_1 = environmentGroup.length === 0 ? p.name : environmentGroup.join(" ");
                    if (name_1 in mapping)
                        mapping[name_1].push(p.index);
                    else {
                        groups++;
                        mapping[name_1] = [p.index];
                    }
                }
            }
            res.set(new Search(profile, mapping));
            $(caption).text("Function filtered to " + found + " / " + profile.length + " entries, " +
                (groups === found ? "not grouped." : groups + " groups."));
        }
    });
    var body = React.createElement("table", { width: "100%", style: "padding-bottom: 17px;" },
        React.createElement("tr", null,
            React.createElement("td", { width: "100%" }, input),
            React.createElement("td", { style: "padding-left:6px;padding-right: 6px;" }, searchHelp(input))),
        React.createElement("tr", null,
            React.createElement("td", null, caption)));
    return [body, res];
}
function searchHelp(input) {
    var examples = [["Only the last run", "run(0)"],
        ["Named 'Main'", "named(\"Main\")"],
        ["Group by file extension", "named(/(\\.[_0-9a-z]+)$/)"],
        ["No dependencies (an input)", "leaf()"],
        ["Didn't change when it last rebuilt", "unchanged()"],
        ["Ran 'gcc'", "command(\"gcc\")"]
    ];
    var f = function (code) { return function () {
        $(input).val(function (i, x) { return x + (x === "" ? "" : " && ") + code; });
        $(input).trigger("change");
    }; };
    var dropdown = React.createElement("div", { class: "dropdown", style: "display:none;" },
        React.createElement("ul", { style: "padding-left:30px;" }, examples.map(function (_a) {
            var desc = _a[0], code = _a[1];
            return React.createElement("li", null,
                React.createElement("a", { onclick: f(code) },
                    React.createElement("tt", null, code)),
                " ",
                React.createElement("span", { class: "note" }, desc));
        })));
    var arrow_down = React.createElement("span", { style: "vertical-align:middle;font-size:80%;" }, "\u25BC");
    var arrow_up = React.createElement("span", { style: "vertical-align:middle;font-size:80%;display:none;" }, "\u25B2");
    var show_inner = function () { $(dropdown).toggle(); $(arrow_up).toggle(); $(arrow_down).toggle(); };
    return React.createElement("div", null,
        React.createElement("button", { style: "white-space:nowrap;padding-top:5px;padding-bottom:5px;", onclick: show_inner },
            React.createElement("b", { style: "font-size:150%;vertical-align:middle;" }, "+"),
            "\u00A0 Filter and Group \u00A0",
            arrow_down,
            arrow_up),
        dropdown);
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
// Stuff that Shake generates and injects in
function untraced(p) {
    return Math.max(0, p.execution - sum(p.traces.map(function (t) { return t.stop - t.start; })));
}
/////////////////////////////////////////////////////////////////////
// BASIC UI TOOLKIT
var Prop = /** @class */ (function () {
    function Prop(val) {
        this.val = val;
        this.callback = function () { return; };
    }
    Prop.prototype.get = function () { return this.val; };
    Prop.prototype.set = function (val) {
        this.val = val;
        this.callback(val);
    };
    Prop.prototype.event = function (next) {
        var old = this.callback;
        this.callback = function (val) { old(val); next(val); };
        next(this.val);
    };
    Prop.prototype.map = function (f) {
        var res = new Prop(f(this.get()));
        this.event(function (a) { return res.set(f(a)); });
        return res;
    };
    return Prop;
}());
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
function showInt(x) {
    // From https://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    // Show, with commas
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
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
    for (var _i = 0, xs_1 = xs; _i < xs_1.length; _i++) {
        var x = xs_1[_i];
        res += x;
    }
    return res;
}
function compareFst(a, b) {
    return a[0] - b[0];
}
function compareSnd(a, b) {
    return a[1] - b[1];
}
function sortOn(xs, f) {
    return xs.map(function (x) { return pair(f(x), x); }).sort(compareFst).map(snd);
}
function last(xs) {
    return xs[xs.length - 1];
}
function maximum(xs, start) {
    var res = start;
    for (var _i = 0, xs_2 = xs; _i < xs_2.length; _i++) {
        var x = xs_2[_i];
        if (x > res)
            res = x;
    }
    return res;
}
function minimum(xs, start) {
    var res = start;
    for (var _i = 0, xs_3 = xs; _i < xs_3.length; _i++) {
        var x = xs_3[_i];
        if (res === undefined || x < res)
            res = x;
    }
    return res;
}
function pair(a, b) {
    return [a, b];
}
function fst(_a) {
    var x = _a[0], _ = _a[1];
    return x;
}
function snd(_a) {
    var _ = _a[0], x = _a[1];
    return x;
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
function concat(xss) {
    var res = [];
    for (var _i = 0, xss_1 = xss; _i < xss_1.length; _i++) {
        var xs = xss_1[_i];
        for (var _a = 0, xs_4 = xs; _a < xs_4.length; _a++) {
            var x = xs_4[_a];
            res.push(x);
        }
    }
    return res;
}
function concatNub(xss) {
    var res = [];
    var seen = {};
    for (var _i = 0, xss_2 = xss; _i < xss_2.length; _i++) {
        var xs = xss_2[_i];
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
function insertArraySorted(xs, x, compare) {
    var start = 0;
    var stop = xs.length - 1;
    var middle = 0;
    while (start <= stop) {
        middle = Math.floor((start + stop) / 2);
        if (compare(xs[middle], x) > 0)
            stop = middle - 1;
        else
            start = middle + 1;
    }
    xs.splice(start, 0, x);
    return xs;
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
    var children = [];
    for (var _a = 0, _children_1 = _children; _a < _children_1.length; _a++) {
        var child = _children_1[_a];
        children.push(Array.isArray(child) ? child : [child]);
    }
    var element = document.createElement(type);
    for (var name_2 in props || {}) {
        if (name_2.substr(0, 2) === "on")
            element.addEventListener(name_2.substr(2), props[name_2]);
        else
            element.setAttribute(name_2, props[name_2]);
    }
    for (var _b = 0, _c = concat(children); _b < _c.length; _b++) {
        var child = _c[_b];
        var c = typeof child === "object" ? child : document.createTextNode(child.toString());
        element.appendChild(c);
    }
    return element;
}
// How .tsx gets desugared
var React = { createElement: createElement };
function reportCmdPlot(profile) {
    // first find the end point
    var runs = findRuns(profile);
    if (runs.length === 0) {
        return React.createElement("div", null,
            React.createElement("h2", null, "No data found"),
            React.createElement("p", null, "The Shake database contains no rules which ran traced commands."),
            React.createElement("p", null,
                "You can populate this information by using ",
                varLink("cmd"),
                " or wrapping your ",
                React.createElement("tt", null, "IO"),
                " actions in ",
                varLink("traced"),
                "."));
    }
    var combo = React.createElement("select", null,
        runs.map(function (_a, i) {
            var run = _a[0], time = _a[1];
            return React.createElement("option", null,
                run === 0 ? "Latest run" : run + " run" + plural(run) + " ago",
                " (" + showTime(time) + ") ",
                i === 0 ? "" : " - may be incomplete");
        }),
        ";");
    var warning = React.createElement("i", null);
    var plot = React.createElement("div", { style: "width:100%; height:100%;" });
    var plotData = new Prop([]);
    bindPlot(plot, plotData, {
        legend: { show: true, position: "nw", sorted: "reverse" },
        series: { stack: true, lines: { fill: 1, lineWidth: 0 } },
        yaxis: { min: 0 },
        xaxis: { tickFormatter: showTime }
    });
    function setPlotData(runsIndex) {
        var _a = runs[runsIndex], run = _a[0], end = _a[1];
        var profileRun = profile.filter(function (p) { return p.built === run; });
        // Make sure we max(0,) every step in the process, in case one does parallelism of threads
        var missing = sum(profileRun.map(untraced));
        $(warning).text(missing < 1 ? "" : "Warning: " + showTime(missing) + " of execution was not traced.");
        var series = calcPlotData(end, profileRun, 100);
        var res = [];
        for (var s in series)
            res.push({ label: s, data: series[s].map(function (x, i) { return pair(end * i / 100, x); }) });
        plotData.set(res);
    }
    setPlotData(0);
    $(combo).change(function () { return setPlotData($(combo).val()); });
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { width: "100%", style: "text-align:center;" },
                React.createElement("h2", null, "Number of commands executing over time")),
            React.createElement("td", null, combo)),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%", colspan: "2" }, plot)),
        React.createElement("tr", null,
            React.createElement("td", { colspan: "2", style: "text-align:center;" },
                "Time since the start of building. ",
                warning)));
}
// Find which runs had traced commands and when the last stopped, sort so most recent first
function findRuns(profile) {
    var runs = {};
    for (var _i = 0, profile_4 = profile; _i < profile_4.length; _i++) {
        var p = profile_4[_i];
        if (p.traces.length > 0) {
            var old = runs[p.built];
            var end = last(p.traces).stop;
            runs[p.built] = old === undefined ? end : Math.max(old, end);
        }
    }
    var runsList = [];
    for (var i in runs)
        runsList.push(pair(Number(i), runs[i]));
    runsList.sort(compareFst);
    return runsList;
}
function calcPlotData(end, profile, buckets) {
    var ans = {};
    for (var _i = 0, profile_5 = profile; _i < profile_5.length; _i++) {
        var p = profile_5[_i];
        for (var _a = 0, _b = p.traces; _a < _b.length; _a++) {
            var t = _b[_a];
            var xs = void 0;
            if (t.command in ans)
                xs = ans[t.command];
            else {
                xs = [];
                for (var i = 0; i < buckets; i++)
                    xs.push(0); // fill with 1 more element, but the last bucket will always be 0
                ans[t.command] = xs;
            }
            var start = t.start * buckets / end;
            var stop_1 = t.stop * buckets / end;
            if (Math.floor(start) === Math.floor(stop_1))
                xs[Math.floor(start)] += stop_1 - start;
            else {
                for (var j = Math.ceil(start); j < Math.floor(stop_1); j++)
                    xs[j]++;
                xs[Math.floor(start)] += Math.ceil(start) - start;
                xs[Math.floor(stop_1)] += stop_1 - Math.floor(stop_1);
            }
        }
    }
    return ans;
}
function reportCmdTable(profile, search) {
    var columns = [{ field: "name", label: "Name", width: 200 },
        { field: "count", label: "Count", width: 65, alignRight: true, show: showInt },
        { field: "total", label: "Total", width: 75, alignRight: true, show: showTime },
        { field: "average", label: "Average", width: 75, alignRight: true, show: showTime },
        { field: "max", label: "Max", width: 75, alignRight: true, show: showTime }
    ];
    return newTable(columns, search.map(cmdData), "total", true);
}
function cmdData(search) {
    var res = {};
    search.forEachProfile(function (p) {
        return p.traces.forEach(function (t) {
            var time = t.stop - t.start;
            if (!(t.command in res))
                res[t.command] = { count: 1, total: time, max: time };
            else {
                var ans = res[t.command];
                ans.count++;
                ans.total += time;
                ans.max = Math.max(ans.max, time);
            }
        });
    });
    var res2 = [];
    for (var i in res)
        res2.push(__assign({ name: i, average: res[i].total / res[i].count }, res[i]));
    return res2;
}
function reportParallelism(profile) {
    // now simulate for -j1 .. -j24
    var plotData = [{ label: "Realistic (based on current dependencies)", data: [], color: "#3131a7" },
        { label: "Ideal (if no dependencies and perfect speedup)", data: [], color: "green" },
        { label: "Gap", data: [], color: "orange" }
    ];
    var threads1;
    for (var threads = 1; threads <= 24; threads++) {
        var taken = simulateThreads(profile, threads)[0];
        if (threads === 1)
            threads1 = taken;
        plotData[0].data.push([threads, taken]);
        plotData[1].data.push([threads, threads1 / threads]);
        plotData[2].data.push([threads, Math.max(0, taken - (threads1 / threads))]);
    }
    var plot = React.createElement("div", { style: "width:100%; height:100%;" });
    bindPlot(plot, new Prop(plotData), {
        xaxis: { tickDecimals: 0 },
        yaxis: { min: 0, tickFormatter: showTime }
    });
    return React.createElement("table", { class: "fill" },
        React.createElement("tr", null,
            React.createElement("td", { style: "text-align:center;" },
                React.createElement("h2", null, "Time to build at different number of threads"))),
        React.createElement("tr", null,
            React.createElement("td", { height: "100%" }, plot)),
        React.createElement("tr", null,
            React.createElement("td", { style: "text-align:center;" }, "Number of threads available.")));
}
// Simulate running N threads over the profile, return:
// [total time take, point at which each entry kicked off]
function simulateThreads(profile, threads) {
    // How far are we through this simulation
    var timestamp = 0;
    // Who is currently running, with the highest seconds FIRST
    var running = [];
    var started = [];
    // Things that are done
    var ready = profile.filter(function (x) { return x.depends.length === 0; });
    var waiting = profile.map(function (x) { return x.depends.length; }); // number I am waiting on before I am done
    function runningWait() {
        var _a = running.pop(), ind = _a[0], time = _a[1];
        timestamp = time;
        for (var _i = 0, _b = profile[ind].rdepends; _i < _b.length; _i++) {
            var d = _b[_i];
            waiting[d]--;
            if (waiting[d] === 0)
                ready.push(profile[d]);
        }
    }
    while (true) {
        // Queue up as many people as we can
        while (running.length < threads && ready.length > 0) {
            var p = ready.pop();
            started[p.index] = timestamp;
            insertArraySorted(running, [p.index, timestamp + p.execution], function (a, b) { return b[1] - a[1]; });
        }
        if (running.length === 0) {
            if (maximum(waiting, 0) > 0)
                throw new Error("Failed to run all tasks");
            return [timestamp, started];
        }
        runningWait();
    }
}
function reportRebuild(profile, search) {
    var depth = [];
    for (var _i = 0, profile_6 = profile; _i < profile_6.length; _i++) {
        var p_1 = profile_6[_i];
        depth[p_1.index] = maximum(p_1.depends.map(function (d) { return depth[d] + 1; }), 0);
    }
    var ind = sortOn(search.get().mapProfile(function (p, _) { return p.index; }), function (i) { return -depth[i]; })[0];
    var p = profile[ind];
    function f(p) {
        var res = [];
        while (p.depends.length !== 0) {
            var ds = sortOn(p.depends.slice(), function (i) { return -depth[i]; });
            res.push(React.createElement("li", null,
                React.createElement("select", { style: "width:400px;" }, ds.slice(0, 1).map(function (x) { return React.createElement("option", null, profile[x].name); }))));
            p = profile[ds[0]];
        }
        return res;
    }
    return React.createElement("div", null,
        React.createElement("h2", null, "Why did it rebuild?"),
        React.createElement("p", null,
            "Rule ",
            p.name + " " + (p.built === 0 ? "rebuild in the last run" : "did not rebuild")),
        React.createElement("ul", null, f(p)));
}
function reportRuleTable(profile, search) {
    var ptimes = calcETimes(profile, 24);
    var columns = [{ field: "name", label: "Name", width: 400 },
        { field: "count", label: "Count", width: 65, alignRight: true, show: showInt },
        { field: "leaf", label: "Leaf", width: 60, alignRight: true },
        { field: "run", label: "Run", width: 50, alignRight: true },
        { field: "changed", label: "Change", width: 60, alignRight: true },
        { field: "time", label: "Time", width: 75, alignRight: true, show: showTime },
        { field: "etime", label: "ETime", width: 75, alignRight: true, show: showTime },
        { field: "untraced", label: "Untraced", width: 100, alignRight: true, show: showTime }
    ];
    return newTable(columns, search.map(function (s) { return ruleData(ptimes, s); }), "time", true);
}
// Calculate the exclusive time of each rule at some number of threads
function calcETimes(profile, threads) {
    var _a = simulateThreads(profile, threads), _ = _a[0], started = _a[1];
    var starts = started.map(function (s, i) { return pair(i, s); }).sort(compareSnd);
    var costs = starts.map(function (_a, i) {
        var ind = _a[0], start = _a[1];
        // find out who else runs before I finish
        var execution = profile[ind].execution;
        var end = start + execution;
        var overlap = 0; // how much time I am overlapped for
        for (var j = i + 1; j < starts.length; j++) {
            var _b = starts[j], jInd = _b[0], jStarts = _b[1];
            if (jStarts > end)
                break;
            overlap += Math.min(end - jStarts, profile[starts[j][0]].execution);
        }
        return pair(ind, execution === 0 ? 0 : execution * (execution / (execution + overlap)));
    });
    var res = [];
    for (var _i = 0, costs_1 = costs; _i < costs_1.length; _i++) {
        var _b = costs_1[_i], ind = _b[0], cost = _b[1];
        res[ind] = cost;
    }
    return res;
}
function ruleData(etimes, search) {
    return search.mapProfiles(function (ps, name) { return ({
        name: name,
        count: ps.length,
        leaf: ps.every(function (p) { return p.depends.length === 0; }),
        run: minimum(ps.map(function (p) { return p.built; })),
        changed: ps.some(function (p) { return p.built === p.changed; }),
        time: sum(ps.map(function (p) { return p.execution; })),
        etime: sum(ps.map(function (p) { return etimes[p.index]; })),
        untraced: sum(ps.map(untraced))
    }); });
}
function reportSummary(profile) {
    var countLast = 0; // number of rules run in the last run
    var highestRun = 0; // highest run you have seen (add 1 to get the count of runs)
    var sumExecution = 0; // build time in total
    var sumExecutionLast = 0; // build time in total
    var countTrace = 0;
    var countTraceLast = 0; // traced commands run
    var maxTraceStopLast = 0; // time the last traced command stopped
    var criticalPath = []; // the critical path to any element
    var maxCriticalPath = 0; // the highest value in criticalPath
    for (var _i = 0, profile_7 = profile; _i < profile_7.length; _i++) {
        var p = profile_7[_i];
        sumExecution += p.execution;
        highestRun = Math.max(highestRun, p.changed); // changed is always greater or equal to built
        countTrace += p.traces.length;
        if (p.built === 0) {
            sumExecutionLast += p.execution;
            countLast++;
            countTraceLast += p.traces.length;
            if (p.traces.length > 0)
                maxTraceStopLast = Math.max(maxTraceStopLast, last(p.traces).stop);
        }
        var cost = maximum(p.depends.map(function (i) { return criticalPath[i]; }), 0) + p.execution;
        maxCriticalPath = Math.max(cost, maxCriticalPath);
        criticalPath[p.index] = cost;
    }
    return React.createElement("div", null,
        React.createElement("h2", null, "Totals"),
        React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Runs:"),
                " ",
                showInt(highestRun + 1),
                " ",
                React.createElement("span", { class: "note" }, "number of times Shake has been run.")),
            React.createElement("li", null,
                React.createElement("b", null, "Rules:"),
                " ",
                showInt(profile.length),
                " (",
                showInt(countLast),
                " in last run) ",
                React.createElement("span", { class: "note" }, "number of defined rules, e.g. individual files.")),
            React.createElement("li", null,
                React.createElement("b", null, "Traced:"),
                " ",
                showInt(countTrace),
                " (",
                showInt(countTraceLast),
                " in last run)",
                React.createElement("span", { class: "note" },
                    "number of calls to ",
                    varLink("cmd"),
                    " or ",
                    varLink("traced"),
                    "."))),
        React.createElement("h2", null, "Performance"),
        React.createElement("ul", null,
            React.createElement("li", null,
                React.createElement("b", null, "Build time:"),
                " ",
                showTime(sumExecution),
                " ",
                React.createElement("span", { class: "note" }, "how long a complete build would take single threaded.")),
            React.createElement("li", null,
                React.createElement("b", null, "Last build time:"),
                " ",
                showTime(maxTraceStopLast),
                " ",
                React.createElement("span", { class: "note" }, "how long the last build take.")),
            React.createElement("li", null,
                React.createElement("b", null, "Parallelism:"),
                " ",
                (maxTraceStopLast === 0 ? 0 : sumExecutionLast / maxTraceStopLast).toFixed(2),
                " ",
                React.createElement("span", { class: "note" }, "average number of commands executing simultaneously in the last build.")),
            React.createElement("li", null,
                React.createElement("b", null, "Critical path:"),
                " ",
                showTime(maxCriticalPath),
                " ",
                React.createElement("span", { class: "note" }, "how long it would take on infinite CPUs."))));
}
