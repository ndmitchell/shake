
function reportRuleTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    const [total, started] = simulateThreads(24, profile);
    const ptimes = slowestParallel2(profile, total, started);
    const columns: Column[] =
        [ {field: "name", label: "Name", width: 400}
        , {field: "count", label: "Count", width: 65, alignRight: true, show: showInt}
        , {field: "leaf", label: "Leaf", width: 60, alignRight: true}
        , {field: "run", label: "Run", width: 50, alignRight: true}
        , {field: "changed", label: "Change", width: 60, alignRight: true}
        , {field: "time", label: "Time", width: 75, alignRight: true, show: showTime}
        , {field: "ptime", label: "PTime", width: 85, alignRight: true, show: showTime}
        , {field: "untraced", label: "Untraced", width: 100, alignRight: true, show: showTime}
        ];
    return newTable(columns, search.map(s => ruleData(ptimes, s)), "time", true);
}

function slowestParallel2(profile: Profile[], total: seconds, started: seconds[]): seconds[] {
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
    const res: seconds[] = [];
    for (const [ind, cost] of costs)
        res[ind] = cost;
    return res;
}

function ruleData(ptimes: seconds[], search: Search): object[] {
    const res = [];
    search.forEachProfiles((ps, name) =>
        res.push({
            name,
            count: ps.length,
            leaf: ps.every(p => p.depends.length === 0),
            run: minimum(ps.map(p => p.built)),
            changed: ps.some(p => p.built === p.changed),
            time: sum(ps.map(p => p.execution)),
            ptime: sum(ps.map(p => ptimes[p.index])),
            untraced: sum(ps.map(untraced))
        })
    );
    return res;
}
