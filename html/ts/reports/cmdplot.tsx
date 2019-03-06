
function reportCmdPlot(profile: Profile[]): HTMLElement {
    // first find the end point
    let end = 0;
    for (const p of profile) {
        if (p.traces.length > 0)
            end = Math.max(end, p.traces[p.traces.length - 1].stop);
    }

    const xs = plotData(end, profile, 100);
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
        const res = <div style="width:100%; height:100%;"></div>;
        const update = () =>
            $.plot($(res), ys, {
                legend: { show: true, position: "nw", sorted: "reverse" },
                // tslint:disable-next-line: object-literal-sort-keys
                series: { stack: true, lines: { fill: 1, lineWidth: 0 } },
                yaxis: { min: 0 },
                xaxis: { tickFormatter: i => showTime(end * i / 100) }
            });
        // do it in a timeout because it must be attached first
        window.setTimeout(update, 1);
        window.onresize = update;
        return res;
    }
}

function plotData(end: seconds, profile: Profile[], buckets: int): MapString<{ items: number[], back: color }> {
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
