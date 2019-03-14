
function reportRebuild(profile: Profile[], search: Prop<Search>): HTMLElement {
    const depth: int[] = [];
    for (const p of profile) {
        depth[p.index] = maximum(p.depends.flat().map(d => depth[d] + 1), 0);
    }

    const ind: pindex = sortOn(search.get().mapProfile((p, _) => p.index), i => -depth[i])[0];
    const p = profile[ind];

    function f(p: Profile): HTMLElement[] {
        const res = [];
        while (p.depends.length !== 0) {
            const ds = sortOn(p.depends.flat(), i => -depth[i]);
            res.push(<li><select style="width:400px;">{ds.slice(0, 1).map(x => <option>{profile[x].name}</option>)}</select></li>);
            p = profile[ds[0]];
        }
        return res;
    }

    return <div>
        <h2>Why did it rebuild?</h2>
        <p>
            Rule {p.name + " " + (p.built === 0 ? "rebuild in the last run" : "did not rebuild")}
        </p>
        <ul>
            {f(p)}
        </ul>
    </div>;
}
