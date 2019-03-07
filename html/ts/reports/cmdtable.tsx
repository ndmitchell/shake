
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
    public sort(x: string, ord: boolean): void;
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
    const table = new DGTable({
            columns: [
                {name: "name", label: "Name", width: "200px"},
                {name: "count", label: "Count", width: "75px", cellClasses: "right"},
                {name: "total", label: "Total", width: "75px", cellClasses: "right"},
                {name: "average", label: "Average", width: "75px", cellClasses: "right"},
                {name: "max", label: "Max", width: "75px", cellClasses: "right"}
            ],
            adjustColumnWidthForSortArrow: false,
            width: DGTable.Width.SCROLL,
            cellFormatter: (val: any, colname: string) =>
                colname === "count" ? showInt(val) :
                colname === "total" || colname === "average" || colname === "max" ? showTime(val) :
                val
        });

    $(table.el).css("height", "100%");
    window.setTimeout(() => {
        table.render();
        table.tableHeightChanged();
        const res2 = [];
        for (const i in res)
            res2.push({name: i, average: res[i].total / res[i].count, ...res[i]});
        table.addRows(res2);
        table.sort("time", true);
        table.render();
    }, 1);
    $(window).on("resize", () => {
        if ($(table.el).is(":visible"))
            table.tableHeightChanged();
    });
    const ret = <div style="height:100%;width:100%;"></div>;
    $(ret).append(table.el);
    return ret;
}
