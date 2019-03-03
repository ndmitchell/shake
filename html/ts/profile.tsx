
function profileRoot(): HTMLElement {
    const [s, search] = createSearch(profile);
    const t = createTabs([["Summary", () => reportSummary(profile, search)], ["Summary", () => <div>Here</div>]]);
    return <div style="background-color:#e8e8e8;">{s}<br/>{t}</div>;
}


function createTabs(xs: Array<[string, () => HTMLElement]>): HTMLElement {
    const bodies = xs.map(x => lazy(x[1]));
    const body = <div></div>;
    let lbls = [];
    const f = (i: int) => () => {
        $(body).empty().append(bodies[i]());
        lbls.map((x, j) => $(x).toggleClass("active", i === j));
    };
    lbls = xs.map((x, i) => <a onclick={f(i)}>{x[0]}</a>);
    f(0)();
    return <div>
        <div class="tabstrip">{lbls}</div>
        <div style="background-color:white;padding-top:5px;"><div>{body}</div></div>
    </div>;
}
