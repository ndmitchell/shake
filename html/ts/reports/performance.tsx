
function reportPerformance(profile: Profile[], search: Prop<Search>): HTMLElement {
    return <div>
        {slowestParallel(profile)}
    </div>;
}

function showItems(total: seconds, items: Array<[seconds, string]>): HTMLElement {
    const f = ([t, s]: [seconds, string]) => showTime(t) + ", " + showPerc(t / total) + ", " + s;
    return <ul>
        {items.map(x => <li>{f(x)}</li>)}
        <li style="font-weight:bold;">{f([sum(items.map(x => x[0])), "TOTAL"])}</li>
    </ul>;
}

function slowestParallel(profile: Profile[]): HTMLElement {
    // now simulate for -j1 .. -j20
    const plot: dataSeries[] = [{label: "Time", data: [], color: "blue"}];
    let started: [seconds, seconds[]];
    for (let threads = 1; threads <= 20; threads++) {
        started = simulateThreads(threads, profile);
        plot[0].data.push([threads, started[0]]);
    }

    const res = <div style="width:100%; height:100px;"></div>;
    const update = () =>
        $.plot($(res), plot, {
            xaxis: { tickDecimals: 0 },
            yaxis: { min: 0, tickFormatter: showTime }
        });
    // do it in a timeout because it must be attached first
    window.setTimeout(update, 1);
    window.onresize = update;
    return <div>
        <b>Parallelism impact</b>
        {res}
        <b>Slowest parallel</b>
        {slowestParallel2(profile, started[0], started[1])}
    </div>;
}

function slowestParallel2(profile: Profile[], total: seconds, started: seconds[]): HTMLElement {
    const starts = started.map((s, i) => pair(i, s)).sort((a, b) => a[1] - b[1]);
    const costs = starts.map(([ind, start], i) => {
        // find out who else runs before I finish
        const execution = profile[ind].execution;
        const end = start + execution;
        let overlap = 0; // how much time I am overlapped for
        for (let j = i + 1; j < starts.length; j++) {
            const [jInd, jStarts] = starts[j];
            if (jStarts > end) break;
            overlap += Math.min(end - jStarts, profile[starts[j][0]].execution);
        }
        return pair(ind, execution === 0 ? 0 : execution * (execution / (execution + overlap)));
    });
    const res = costs.sort((a, b) => b[1] - a[1]).slice(0, 10).map(x => pair(x[1], profile[x[0]].name));
    return showItems(total, res);
}

function simulateThreads(threads: int, profile: Profile[]): [seconds, seconds[]] {
    // How far are we through this simulation
    let timestamp: seconds = 0;

    // Who is currently running, with the highest seconds FIRST
    const running: Array<[pindex, seconds]> = [];
    const started: seconds[] = [];

    // Things that are done
    const ready: Profile[] = profile.filter(x => x.depends.length === 0);
    const waiting: int[] = profile.map(x => x.depends.length) ; // number I am waiting on before I am done

    function runningWait(): void {
        const [ind, time] = running.pop();
        timestamp = time;
        for (const d of profile[ind].rdepends) {
            waiting[d]--;
            if (waiting[d] === 0)
                ready.push(profile[d]);
        }
    }

    while (true) {
        // Queue up as many people as we can
        while (running.length < threads && ready.length > 0) {
            const p = ready.pop();
            started[p.index] = timestamp;
            insertArraySorted(running, [p.index, timestamp + p.execution], (a, b) => b[1] - a[1]);
        }
        if (running.length === 0) {
            if (maximum(waiting, 0) > 0)
                throw new Error("Failed to run all tasks");
            return [timestamp, started];
        }
        runningWait();
    }
}
