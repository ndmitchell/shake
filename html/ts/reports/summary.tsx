
function reportSummary(profile: Profile[]): HTMLElement {
    let countLast: int = 0; // number of rules run in the last run
    let highestRun: timestamp = 0; // highest run you have seen (add 1 to get the count of runs)
    let sumExecution: seconds = 0; // build time in total
    let sumExecutionLast: seconds = 0; // build time in total
    let countTrace: int = 0; let countTraceLast: int = 0; // traced commands run
    let maxTraceStopLast: seconds = 0; // time the last traced command stopped
    const criticalPath: seconds[] = []; // the critical path to any element
    let maxCriticalPath: seconds = 0; // the highest value in criticalPath

    for (const p of profile) {
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
        const cost = maximum(p.depends.map(i => criticalPath[i]), 0) + p.execution;
        maxCriticalPath = Math.max(cost, maxCriticalPath);
        criticalPath[p.index] = cost;
    }

    return <div>
        <h2>Totals</h2>
        <ul>
            <li><b>Runs:</b> {showInt(highestRun + 1)} <span class="note">number of times Shake has been run.</span></li>
            <li><b>Rules:</b> {showInt(profile.length)} ({showInt(countLast)} in last run) <span class="note">number of defined rules, e.g. individual files.</span></li>
            <li><b>Traced:</b> {showInt(countTrace)} ({showInt(countTraceLast)} in last run)
                <span class="note">number of calls to {varLink("cmd")} or {varLink("traced")}.</span>
            </li>
        </ul>
        <h2>Performance</h2>
        <ul>
            <li><b>Build time:</b> {showTime(sumExecution)} <span class="note">how long a complete build would take single threaded.</span></li>
            <li><b>Last build time:</b> {showTime(maxTraceStopLast)} <span class="note">how long the last build take.</span></li>
            <li><b>Parallelism:</b> {(maxTraceStopLast === 0 ? 0 : sumExecutionLast / maxTraceStopLast).toFixed(2)} <span class="note">average number of commands executing simultaneously in the last build.</span></li>
            <li><b>Critical path:</b> {showTime(maxCriticalPath)} <span class="note">how long it would take on infinite CPUs.</span></li>
        </ul>
    </div>;
}
