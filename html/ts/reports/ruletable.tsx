
function reportRuleTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    const columns: Column[] =
        [ {field: "name", label: "Name", width: 400}
        , {field: "count", label: "Count", width: 75, alignRight: true, show: showInt}
        , {field: "leaf", label: "Leaf", width: 60, alignRight: true}
        , {field: "run", label: "Run", width: 50, alignRight: true}
        , {field: "changed", label: "Change", width: 60, alignRight: true}
        , {field: "time", label: "Time", width: 75, alignRight: true, show: showTime}
        , {field: "ptime", label: "PTime", width: 85, alignRight: true, show: showTime}
        ];
    return newTable(columns, search.map(ruleData), "time", true);
}

function ruleData(search: Search): object[] {
    const res = [];
    search.forEachProfiles((ps, name) =>
        res.push({
            name,
            count: ps.length,
            leaf: ps.every(p => p.depends.length === 0),
            run: minimum(ps.map(p => p.built)),
            changed: ps.some(p => p.built === p.changed),
            time: sum(ps.map(p => p.execution)),
            ptime: 0
        })
    );
    return res;
}
