
function reportRuleTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    return cmdRuleTable(search.get());
}

function cmdRuleTable(search: Search): HTMLElement {
    const trs = [];
    search.forEachProfiles((ps, group) =>
        trs.push({group, count: ps.length})
    );

    const table = new DGTable({
            columns: [
                {name: "group", label: "Group", width: "300px"},
                {name: "count", label: "Count", width: "100px", cellClasses: "right"}
            ],
            height: 500,
            width: DGTable.Width.SCROLL,
            cellFormatter: (val: any, colname: string) =>
                colname === "count" ? showInt(val) :
                val
        });

    window.setTimeout(() => {
        table.render();
        table.setRows(trs, true);
        $(table.el).css("height", "100%");
        table.tableHeightChanged();
        table.render();
    }, 1);
    $(window).on("resize", () => table.tableHeightChanged());
    const ret = <div style="height:100%;width:100%;"></div>;
    $(ret).append(table.el);
    return ret;
}
