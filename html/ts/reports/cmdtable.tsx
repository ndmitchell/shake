
function reportCmdTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    return cmdTableData(search.get());
}

function cmdTableData(search: Search): HTMLElement {
    const res: MapString< {count: int, time: seconds} > = {};
    search.forEachProfile(p =>
        p.traces.forEach(t => {
            const time = t.stop - t.start;
            if (!(t.command in res))
                res[t.command] = {count: 1, time};
            else {
                res[t.command].count++;
                res[t.command].time += time;
            }
        })
    );
    const trs = [];
    for (const i in res)
        trs.push(<tr><td>{i}</td><td>{res[i].count}</td><td>{showTime(res[i].time)}</td></tr>);
    return (
        <table class="data">
            <tr class="header">
                <td>Name</td>
                <td>Count</td>
                <td>Time</td>
            </tr>
            {trs}
        </table>);
}
