
function reportCmdTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    return cmdTableData(search.get());
}

function cmdTableData(search: Search): HTMLElement {
    const res: MapString< {count: int, total: seconds, max: seconds} > = {};
    search.forEachProfile(p =>
        p.traces.forEach(t => {
            const time = t.stop - t.start;
            if (!(t.command in res))
                res[t.command] = {count: 1, total: time, max: time};
            else {
                res[t.command].count++;
                res[t.command].total += time;
                res[t.command].max = Math.max(res[t.command].max, time);
            }
        })
    );
    const columns: Column[] =
        [ {field: "name", label: "Name", width: 200}
        , {field: "count", label: "Count", width: 75, alignRight: true, show: showInt}
        , {field: "total", label: "Total", width: 75, alignRight: true, show: showTime}
        , {field: "average", label: "Average", width: 75, alignRight: true, show: showTime}
        , {field: "max", label: "Max", width: 75, alignRight: true, show: showTime}
        ];
    const res2 = [];
    for (const i in res)
        res2.push({name: i, average: res[i].total / res[i].count, ...res[i]});
    return newTable(columns, new Prop(res2));
}
