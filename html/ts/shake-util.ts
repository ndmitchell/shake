
type key = string | number;

type seconds = number;

type color = string;

type MapString<T> = { [key: string]: T };
type MapNumber<T> = { [key: number]: T };

type int = number;
type MapInt<T> = MapNumber<T>;


/////////////////////////////////////////////////////////////////////
// JQUERY EXTENSIONS

// tslint:disable-next-line: interface-name
interface JQuery {
    enable(x: boolean): JQuery;
}

jQuery.fn.enable = function(x: boolean) {
    // Set the values to enabled/disabled
    return this.each(function() {
        if (x)
            $(this).removeAttr("disabled");
        else
            $(this).attr("disabled", "disabled");
    });
};


/////////////////////////////////////////////////////////////////////
// BROWSER HELPER METHODS

// Given "?foo=bar&baz=1" returns {foo:"bar",baz:"1"}
function uriQueryParameters(s: string): MapString<string> {
    // From https://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    const params: MapString<string> = {};
    const a = /\+/g;  // Regex for replacing addition symbol with a space
    const r = /([^&=]+)=?([^&]*)/g;
    const d = (x: string) => decodeURIComponent(x.replace(a, " "));
    const q = s.substring(1);

    while (true) {
        const e = r.exec(q);
        if (!e) break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
}


/////////////////////////////////////////////////////////////////////
// STRING FORMATTING

function showTime(x: seconds): string {
    function digits(x: seconds) {const s = String(x); return s.length === 1 ? "0" + s : s; }

    if (x >= 3600) {
        x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    } else if (x >= 60) {
        x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    } else
        return x.toFixed(2) + "s";
}

function showPerc(x: number): string {
    return (x * 100).toFixed(2) + "%";
}

function showInt(x: int): string {
    // From https://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
    // Show, with commas
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}

function plural(n: int, not1 = "s", is1 = ""): string {
    return n === 1 ? is1 : not1;
}


/////////////////////////////////////////////////////////////////////
// MISC

function sum(xs: number[]): number {
    let res = 0;
    for (const x of xs)
        res += x;
    return res;
}

function compare(a: number, b: number): number {
    return b - a;
}

function compareFst<A>(a: [number, A], b: [number, A]): number {
    return b[0] - a[0];
}

function sortOn<A>(xs: A[], f: (x: A) => number): A[] {
    return xs.map(x => pair(f(x), x)).sort(compareFst).map(snd);
}

function last<A>(xs: A[]): A {
    return xs[xs.length - 1];
}

function maximum<A>(xs: A[], start: A): A {
    let res: A = start;
    for (const x of xs)
        if (x > res)
            res = x;
    return res;
}

function pair<A, B>(a: A, b: B): [A, B] {
    return [a, b];
}

function fst<A, B>([x, _]: [A, B]): A {
    return x;
}

function snd<A, B>([_, x]: [A, B]): B {
    return x;
}

function testRegExp(r: string | RegExp, s: string): boolean {
    if (typeof r === "string")
        return s.indexOf(r) !== -1;
    else
        return r.test(s);
}

function execRegExp(r: string | RegExp, s: string): string[] {
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}

function listEq<T>(xs: T[], ys: T[]): boolean {
    if (xs.length !== ys.length) return false;
    for (let i = 0; i < xs.length; i++) {
        if (xs[i] !== ys[i])
            return false;
    }
    return true;
}

function cache<K, V>(key: (k: K) => string, op: (k: K) => V): (k: K) => V {
    const store: MapString<V> = {};
    return k => {
        const s = key(k);
        if (!(s in store))
            store[s] = op(k);
        return store[s];
    };
}

function lazy<V>(thunk: () => V): () => V {
    let store: V = null;
    let done = false;
    return () => {
        if (!done) {
            store = thunk();
            done = true;
        }
        return store;
    };
}

function concat<T>(xss: T[][]): T[] {
    const res: T[] = [];
    for (const xs of xss) {
        for (const x of xs) {
            res.push(x);
        }
    }
    return res;
}

function concatNub<T extends key>(xss: T[][]): T[] {
    const res: T[] = [];
    const seen: {} = {};
    for (const xs of xss) {
        for (const x of xs) {
            const v: key = x;
            if (!(v in seen)) {
                (seen as any)[v] = null;
                res.push(x);
            }
        }
    }
    return res;
}

function insertArraySorted<A>(xs: A[], x: A, compare: (a: A, b: A) => number): A[] {
    let start = 0;
    let stop = xs.length - 1;
    let middle = 0;
    while (start <= stop) {
        middle = Math.floor((start + stop) / 2);
        if (compare(xs[middle], x) > 0)
            stop = middle - 1;
        else
            start = middle + 1;
    }
    xs.splice(start, 0, x);
    return xs;
}


// Use JSX with el instead of React.createElement
// Originally from https://gist.github.com/sergiodxa/a493c98b7884128081bb9a281952ef33

// our element factory
function createElement(type: string, props?: MapString<any>, ..._children: any[]) {
    // if _children is an array of array take the first value, else take the full array
    const children: any[][] = [];
    for (const child of _children)
        children.push(Array.isArray(child) ? child : [child]);

    const element = document.createElement(type);

    for (const name in props || {}) {
        if (name.substr(0, 2) === "on")
            element.addEventListener(name.substr(2), props[name]);
        else
            element.setAttribute(name, props[name]);
    }
    for (const child of concat(children)) {
        const c = typeof child === "object" ? child : document.createTextNode(child.toString());
        element.appendChild(c);
    }
    return element;
}

// How .tsx gets desugared
const React = {createElement};
