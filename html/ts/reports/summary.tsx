
function reportSummary(profile: Profile[], search: Prop<Search>): HTMLElement {
    let count: int = 0; // number of rules run
    let countLast: int = 0; // number of rules run in the last run
    let highestRun: timestamp = 0; // highest run you have seen (add 1 to get the count of runs)
    let sumExecution: seconds = 0; // build time in total
    let maxExecution: seconds = 0; // longest build rule
    let maxExecutionName: string = ""; // longest build rule
    let countTrace: int = 0; let countTraceLast: int = 0; // traced commands run
    let sumTrace: seconds = 0; let sumTraceLast: seconds = 0; // time running traced commands
    let maxTrace: seconds = 0; // longest traced command
    let maxTraceName: string = ""; // longest trace command
    let maxTraceStopLast: seconds = 0; // time the last traced command stopped
    const criticalPath: seconds[] = []; // the critical path to any element
    let maxCriticalPath: seconds = 0; // the highest value in criticalPath

    profile.forEach((e, i) => {
        const isLast = e.built === 0;
        count++;
        countLast += isLast ? 1 : 0;
        sumExecution += e.execution;
        maxExecution = Math.max(maxExecution, e.execution);
        if (maxExecution === e.execution) maxExecutionName = e.name;
        highestRun = Math.max(highestRun, e.changed); // changed is always greater or equal to built
        for (const t of e.traces) {
            const time = t.stop - t.start;
            countTrace += 1;
            countTraceLast += isLast ? 1 : 0;
            sumTrace += time;
            sumTraceLast += isLast ? time : 0;
            maxTrace = Math.max(maxTrace, time);
            if (maxTrace === time) maxTraceName = t.command;
            maxTraceStopLast = Math.max(maxTraceStopLast, isLast ? t.stop : 0);
        }
        const p = maximum(e.depends.map(i => criticalPath[i]), 0) + e.execution;
        maxCriticalPath = Math.max(p, maxCriticalPath);
        criticalPath[i] = p;
    });
    const lines =
        [ "This database has tracked " + showInt(highestRun + 1) + " run" + plural(highestRun + 1) + "."
        , "There are " + showInt(count) + " rules (" + showInt(countLast) + " rebuilt in the last run)."
        , "Building required " + showInt(countTrace) + " traced commands (" + showInt(countTraceLast) + " in the last run)."
        , "The total (unparallelised) build time is " + showTime(sumExecution) + " of which " + showTime(sumTrace) + " is traced commands."
        , "The longest rule takes " + showTime(maxExecution) + " (" + maxExecutionName + ") and the longest traced command takes " + showTime(maxTrace) + " (" + maxTraceName + ")."
        , "Last run gave an average parallelism of " + (maxTraceStopLast === 0 ? 0 : sumTraceLast / maxTraceStopLast).toFixed(2) + " times over " + showTime(maxTraceStopLast) + "."
        , "The critical path was " + showTime(maxCriticalPath) + "."
        ];

    return <div>
        <h2>Totals</h2>
        <ul>
            <li><b>Runs:</b> {showInt(highestRun + 1)} <span class="note">number of times Shake has been run.</span></li>
            <li><b>Rules:</b> {showInt(count)} ({showInt(countLast)} in last run) <span class="note">number of defined rules, e.g. individual files.</span></li>
            <li><b>Traced:</b> {showInt(countTrace)} ({showInt(countTraceLast)} in last run)
                <span class="note">number of calls to
                {" "}<tt><a href="https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:cmd">cmd</a></tt> or
                {" "}<tt><a href="https://hackage.haskell.org/package/shake/docs/Development-Shake.html#v:traced">traced</a></tt>.</span>
            </li>
        </ul>
        <h2>Performance</h2>
        <ul>
            <li><b>Build time:</b> {showTime(sumExecution)} <span class="note">how long a complete build would take single threaded.</span></li>
            <li><b>Last build time:</b> {showTime(maxTraceStopLast)} <span class="note">how long the last build take.</span></li>
            <li><b>Parallelism:</b> {(maxTraceStopLast === 0 ? 0 : sumTraceLast / maxTraceStopLast).toFixed(2)} <span class="note">average number of commands executing simultaneously.</span></li>
            <li><b>Critical path:</b> {showTime(maxCriticalPath)} <span class="note">how long it would take on infinite CPUs.</span></li>
        </ul>
    </div>;
}
