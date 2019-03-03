
function reportCmdPlot(profile: Profile[], search: Prop<Search>): HTMLElement {
    const xs = plotData(search.get(), 100);

    const ys: Array<dataSeries & { avg: number }> = [];
    for (const s in xs) {
        const x = xs[s].items;
        const data: Array<[number, number]> = [];
        for (let j = 0; j < x.length; j++)
            data.push([j, x[j]]);
        ys.push({ label: s, /* values:x, */ data, color: xs[s].back, avg: sum(x) / x.length });
    }

    if (ys.length === 0) {
        return <div>No data found</div>;
    } else {
        ys.sort((a, b) => a.avg - b.avg);
        const res = <div style="width:400px; height: 300px;"></div>;
        // do it in a timeout because it must be attached first
        window.setTimeout(() =>
            $.plot($(res), ys, {
                legend: { show: true, position: "nw", sorted: "reverse" },
                // tslint:disable-next-line: object-literal-sort-keys
                series: { stack: true, lines: { fill: 1, lineWidth: 0 } },
                yaxis: { min: 0 },
                xaxis: { tickFormatter: i => showTime(prepared.summary.maxTraceStopLast * i / 100) }
            })
        , 1);
        return res;
    }
}

function plotData(search: Search, buckets: int): MapString<{ items: number[], back: color }> {
    // first find the end point
    let end = 0;
    search.forEachProfile(p => {
        if (p.traces.length > 0)
            end = Math.max(end, p.traces[p.traces.length - 1].stop);
    });

    const ans: MapString<{ items: number[], back: color }> = {};
    search.forEachProfile(p => {
        p.traces.forEach(t => {
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
        });
    });
    return ans;
}
