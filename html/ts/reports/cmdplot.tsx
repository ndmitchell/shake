
function reportCmdPlot(profile: Profile[]): HTMLElement {
    // first find the end point
    const runs = findRuns(profile);

    if (runs.length === 0) {
        return <div>
            <h2>No data found</h2>
            <p>
                The Shake database contains no rules which ran traced commands.
            </p><p>
                You can populate this information
                {" "}by using <a href="https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:cmd"><tt>cmd</tt></a> or
                {" "}wrapping your <tt>IO</tt> actions in
                {" "}<a href="https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:traced"><tt>traced</tt></a>.
            </p>
        </div>;
    }

    const combo = <select>
        {runs.map(([run, time], i) =>
            <option>
                {run === 0 ? "Latest run" : run + " run" + plural(run) + " ago"}
                {" (" + showTime(time) + ") "}
                {i === 0 ? "" : " - may be incomplete"}
            </option>)};
    </select>;

    const warning = <i></i>;
    const plot = <div style="width:100%; height:100%;"></div>;
    let plotData = [];
    const redrawPlot = () =>
        $.plot($(plot), plotData, {
            legend: { show: true, position: "nw", sorted: "reverse" },
            // tslint:disable-next-line: object-literal-sort-keys
            series: { stack: true, lines: { fill: 1, lineWidth: 0 } },
            yaxis: { min: 0 },
            xaxis: { tickFormatter: showTime }
        });
    window.setTimeout(redrawPlot, 1);
    window.onresize = redrawPlot;

    function setPlotData(runsIndex: int) {
        const [run, end] = runs[runsIndex];
        const profileRun = profile.filter(p => p.built === run);
        // Make sure we max(0,) every step in the process, in case one does parallelism of threads
        const missing = sum(profileRun.map(p => Math.max(0, p.execution - sum(p.traces.map(t => t.stop - t.start)))));
        $(warning).text(missing < 1 ? "" : "Warning: " + showTime(missing) + " of execution was not traced.");
        const series = calcPlotData(end, profileRun, 100);
        plotData = [];
        for (const s in series)
            plotData.push({label: s, data: series[s].items.map((x, i) => pair(end * i / 100, x))});
        plotData = sortOn(plotData, x => sum(x.data) / x.data.length);
    }
    setPlotData(0);
    $(combo).change(() => { setPlotData($(combo).val()); redrawPlot(); });

    return <table class="fill">
        <tr>
            <td width="100%" style="text-align:center;"><h2>Traced commands executing over time</h2></td>
            <td>{combo}</td>
        </tr>
        <tr>
            <td height="100%" colspan="2">{plot}</td>
        </tr>
        <tr>
            <td colspan="2" style="text-align:center;">Time since the start of building. {warning}</td>
        </tr>
    </table>;
}

// Find which runs had traced commands and when the last stopped, sort so most recent first
function findRuns(profile: Profile[]): Array<[timestamp, seconds]> {
    const runs: MapInt<seconds> = {};
    for (const p of profile) {
        if (p.traces.length > 0) {
            const old = runs[p.built];
            const end = last(p.traces).stop;
            runs[p.built] = old === undefined ? end : Math.max(old, end);
        }
    }

    const runsList: Array<[timestamp, seconds]> = [];
    for (const i in runs)
        runsList.push(pair(Number(i), runs[i]));
    runsList.sort(compareFst);
    return runsList;
}

function calcPlotData(end: seconds, profile: Profile[], buckets: int): MapString<{ items: number[], back: color }> {
    const ans: MapString<{ items: number[], back: color }> = {};
    for (const p of profile) {
        for (const t of p.traces) {
            let xs: number[];
            if (t.command in ans)
                xs = ans[t.command].items;
            else {
                xs = [];
                for (let i = 0; i < buckets; i++)
                    xs.push(0); // fill with 1 more element, but the last bucket will always be 0
                ans[t.command] = {items: xs, back: null};
            }

            const start = t.start * buckets / end;
            const stop = t.stop * buckets / end;

            if (Math.floor(start) === Math.floor(stop))
                xs[Math.floor(start)] += stop - start;
            else {
                for (let j = Math.ceil(start); j < Math.floor(stop); j++)
                    xs[j]++;
                xs[Math.floor(start)] += Math.ceil(start) - start;
                xs[Math.floor(stop)] += stop - Math.floor(stop);
            }
        }
    }
    return ans;
}
