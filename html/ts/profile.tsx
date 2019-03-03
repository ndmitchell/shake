function profileLoaded(): void {
    $(document.body).empty().append(profileRoot());
}

function profileRoot(): HTMLElement {
    const [s, search] = createSearch(profile);
    const t = createTabs(
        [ ["Summary", () => reportSummary(profile, search)]
        , ["Commands over time", () => reportCmdPlot(profile, search)]
        , ["Commands", () => reportCmdTable(profile, search)]
        , ["Rules", () => reportRuleTable(profile, search)]
        ]);
    return <table class="fill">
        <tr><td style="padding-top: 8px; padding-bottom: 8px;">
            <a href="https://shakebuild.com/" style="font-size: 20px; text-decoration: none; color: #3131a7; font-weight: bold;">
                Shake profile report
            </a>
            <span style="color:gray;white-space:pre;">   - generated at {generated} by Shake v{version}</span>
        </td></tr>
        <tr><td>{s}</td></tr>
        <tr><td height="100%">{t}</td></tr>
    </table>;
}


function createTabs(xs: Array<[string, () => HTMLElement]>): HTMLElement {
    const bodies = xs.map(x => lazy(x[1]));
    const body = <div style="padding:5px;width:100%;height:100%;min-width:150px;min-height:150px;overflow:auto;"></div>;
    let lbls = [];
    const f = (i: int) => () => {
        $(body).empty().append(bodies[i]());
        lbls.map((x, j) => $(x).toggleClass("active", i === j));
    };
    lbls = xs.map((x, i) => <a onclick={f(i)}>{x[0]}</a>);
    f(0)();
    return <table class="fill">
        <tr><td>
            <table width="100%" style="border-spacing:0px;"><tr class="tabstrip">
                <td width="20" class="bottom">&nbsp;</td>
                <td style="padding:0px;">{lbls}</td>
                <td width="100%" class="bottom">&nbsp;</td>
            </tr></table>
        </td></tr>
        <tr height="100%">
            <td style="background-color:white;padding-top:10px;">
                {body}
            </td>
        </tr>
    </table>;
}
