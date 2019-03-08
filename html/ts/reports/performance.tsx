
function reportPerformance(profile: Profile[]): HTMLElement {
    // now simulate for -j1 .. -j20
    const plot: dataSeries[] = [{label: "Time", data: [], color: "blue"}];
    let started: [seconds, seconds[]];
    for (let threads = 1; threads <= 20; threads++) {
        started = simulateThreads(profile, threads);
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
    </div>;
}

// Simulate running N threads over the profile, return:
// [total time take, point at which each entry kicked off]
function simulateThreads(profile: Profile[], threads: int): [seconds, seconds[]] {
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
