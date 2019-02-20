

type Search = MapString<int[]>;

function createSearch(change: (s: Search) => void): HTMLElement {
    return null; // return <input type="text" value="true" />;
}

function fullSearch() : Search
{
    let res = {};
    for (const i in profile)
        res[profile[i].name] = [i];
    return res;
}

function ruleFilter(dat: Prepare, query: string): MapString<Result> {
    queryData = dat;
    const f = readQuery(query);
    const res: MapString<Result> = {};

    for (queryKey = 0; queryKey < dat.original.length; queryKey++) {
        queryVal = dat.original[queryKey];
        queryName = queryVal.name;
        queryGroup = null;
        queryBackColor = null;
        queryTextColor = null;
        if (f()) {
            if (queryGroup === null) queryGroup = queryName;
            if (!(queryGroup in res))
                res[queryGroup] = { items: [queryKey], text: queryTextColor, back: queryBackColor };
            else {
                const c = res[queryGroup];
                c.items.push(queryKey);
                c.text = colorAnd(c.text, queryTextColor);
                c.back = colorAnd(c.back, queryBackColor);
            }
        }
    }
    return res;
}

/////////////////////////////////////////////////////////////////////
// ENVIRONMENT

function readQuery(query: string): () => boolean {
    if (query === "") return () => true;
    let f: () => boolean;
    try {
        f = (new Function("return " + query)) as (() => boolean);
    } catch (e) {
        throw { user: true, name: "parse", query: query, message: e.toString() };
    }
    return () => {
        try {
            return f();
        } catch (e) {
            throw { user: true, name: "execution", query: query, message: e.toString() };
        }
    };
}


// These are global variables mutated/queried by query execution
let queryData: Prepare = {} as Prepare;
let queryKey: int = 0;
let queryVal: ProfileEx = {} as ProfileEx;
let queryName: string = "";
let queryGroup: string = null;
let queryBackColor: color = null;
let queryTextColor: color = null;

function childOf(r: string | RegExp) { return queryData.dependsOnThis(queryKey, r); }
function parentOf(r: string | RegExp) { return queryData.thisDependsOn(queryKey, r); }
function ancestorOf(r: string | RegExp) { return queryData.dependsOnThisTransitive(queryKey, r); }
function descendantOf(r: string | RegExp) { return queryData.thisDependsOnTransitive(queryKey, r); }
function descendentOf(r: string | RegExp) { return descendantOf(r); }

function /* export */ group(x: string): boolean {
    if (queryGroup === null) queryGroup = "";
    queryGroup += (queryGroup === "" ? "" : " ") + x;
    return true;
}

function backColor(c: color, b: boolean = true): boolean {
    if (b)
        queryBackColor = c;
    return true;
}

function textColor(c: color, b = true): boolean {
    if (b === undefined || b)
        queryTextColor = c;
    return true;
}

function rename(from: string, to: string = ""): boolean {
    queryName = queryName.replace(from, to);
    return true;
}

let slowestRule_Cache: string | null = null; // Index of the slowest rule
function slowestRule(): string {
    if (slowestRule_Cache === null)
    {
        let time = -1;
        let name = "";
        for (const p of profile)
        {
            if (p[1] <= time) continue;
            name = p[0];
            time = p[1];
        }
        slowestRule_Cache = name;
    }
    return slowestRule_Cache;
}

function /* export */ leaf(): boolean {
    return queryVal.depends.length === 0;
}

function run(): number;
function run(i: timestamp): boolean;
function run(i?: timestamp): number | boolean {
    if (i === undefined)
        return queryVal.built;
    else
        return queryVal.built === i;
}

function /* export */ unchanged(): boolean {
    return queryVal.changed !== queryVal.built;
}

function named(): string;
function named(r: string | RegExp, groupName?: string): boolean;
function /* export */ named(r?: string | RegExp, groupName?: string): string | boolean {
    if (r === undefined)
        return queryName;

    const res = execRegExp(r, queryName);
    if (res === null) {
        if (groupName === undefined)
            return false;
        else {
            group(groupName);
            return true;
        }
    }
    if (res.length !== 1) {
        for (let i = 1; i < res.length; i++)
            group(res[i]);
    }
    return true;
}

function command(): string;
function command(r: string | RegExp, groupName?: string): boolean;
function /* export */ command(r?: any, groupName?: any): any {
    const n = (queryVal.traces || []).length;
    if (r === undefined)
        return n === 0 ? "" : queryVal.traces[0].command;

    for (const t of queryVal.traces) {
        const res = execRegExp(r, t.command);
        if (res === null)
            continue;
        if (res.length !== 1) {
            for (let j = 1; j < res.length; j++)
                group(res[j]);
        }
        return true;
    }
    if (groupName === undefined)
        return false;
    else {
        group(groupName);
        return true;
    }
}
