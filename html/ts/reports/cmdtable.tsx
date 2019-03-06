
function reportCmdTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    return cmdTableData(search.get());
}

declare class DGTable {
    public static Width: {SCROLL: void};
    public el: HTMLElement;
    constructor(x: any);
    public addRows(x: any[]): void;
    public render(): any;
    public tableHeightChanged(): any;
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
    const table = new DGTable({
            columns: [
                {name: "name", label: "Name", width: "300px"},
                {name: "count", label: "Count", width: "100px", cellClasses: "right"},
                {name: "time", label: "Time", width: "100px", cellClasses: "right"}
            ],
            height: 500,
            sortColumn: {column: "time", descending: true},
            width: DGTable.Width.SCROLL,
            cellFormatter: (val: any, colname: string) =>
                colname === "count" ? showInt(val) :
                colname === "time" ? showTime(val) :
                val
        });

    const trs = [];
    window.setTimeout(() => {
        for (const i in res)
            table.render().addRows([{name: i, ...res[i]}]);
        $(table.el).css("height", "100%");
        table.tableHeightChanged();
        table.render();
    }, 1);
    window.onresize = () => table.tableHeightChanged();
    const ret = <div style="height:100%;width:100%;"></div>;
    $(ret).append(table.el);
    return ret;
}
