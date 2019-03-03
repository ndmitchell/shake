
function reportRuleTable(profile: Profile[], search: Prop<Search>): HTMLElement {
    return cmdRuleTable(search.get());
}

function cmdRuleTable(search: Search): HTMLElement {
    const trs = [];
    search.forEachProfiles((ps, group) =>
        trs.push(<tr><td>{group}</td><td>{ps.length}</td></tr>)
    );
    return (
        <table class="data">
            <tr class="header">
                <td>Name</td>
                <td>Count</td>
                <td>Items</td>
            </tr>
            {trs}
        </table>);
}
