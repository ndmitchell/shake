/*jsl:option explicit*/
"use strict";

type key = string | number;

type seconds = number

type color = string

type MapString<T> = { [key: string]: T }
type MapNumber<T> = { [key: number]: T }

type int = number
type MapInt<T> = MapNumber<T>


/////////////////////////////////////////////////////////////////////
// JQUERY EXTENSIONS

interface JQuery {
    enable(x: boolean): JQuery
}

jQuery.fn.enable = function (x : boolean)
{
    // Set the values to enabled/disabled
    return this.each(function () {
        if (x)
            $(this).removeAttr('disabled');
        else
            $(this).attr('disabled','disabled');
    });
};


/////////////////////////////////////////////////////////////////////
// BROWSER HELPER METHODS

// Given "?foo=bar&baz=1" returns {foo:"bar",baz:"1"}
function uriQueryParameters(s : string) : MapString<string> {
    // From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    const params: MapString<string> = {};
    const a = /\+/g;  // Regex for replacing addition symbol with a space
    const r = /([^&=]+)=?([^&]*)/g;
    const d = function (s: string) { return decodeURIComponent(s.replace(a, " ")); };
    const q = s.substring(1);

    while (true)
    {
        var e = r.exec(q);
        if (!e) break;
        params[d(e[1])] = d(e[2]);
    }
    return params;
};


/////////////////////////////////////////////////////////////////////
// STRING FORMATTING

function showTime(x : seconds) : string
{
    function digits(x:seconds){var s = String(x); return s.length === 1 ? "0" + s : s;}

    if (x >= 3600)
    {
        x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    }
    else if (x >= 60)
    {
        x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    }
    else
        return x.toFixed(2) + "s";
}

function showPerc(x : number) : string
{
    return (x*100).toFixed(2) + "%";
}

function plural(n: int, not1 = "s", is1 = ""): string
{
    return n === 1 ? is1 : not1;
}


/////////////////////////////////////////////////////////////////////
// MISC

function sum(xs : number[]) : number
{
    let res = 0;
    for (const x of xs)
        res += x;
    return res;
}

function testRegExp(r : string | RegExp, s : string) : boolean
{
    if (typeof r === "string")
        return s.indexOf(r) !== -1;
    else
        return r.test(s);
}

function execRegExp(r : string | RegExp, s : string) : string[]
{
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}

function listEq<T>(xs : T[], ys : T[]) : boolean
{
    if (xs.length !== ys.length) return false;
    for (let i = 0; i < xs.length; i++)
    {
        if (xs[i] !== ys[i])
            return false;
    }
    return true;
}

function cache<K, V>(key: (k: K) => string, op: (k: K) => V): (k: K) => V
{
    const store: MapString<V> = {};
    return function (k) {
        const s = key(k);
        if (!(s in store))
            store[s] = op(k);
        return store[s];
    };
}

function mapEq<V>(xs: MapString<V>, ys: MapString<V>) : boolean
{
    function f(a: MapString<V>, b: MapString<V>)
    {
        for (const s in a)
        {
            if (a[s] !== b[s]) return false;
        }
        return true;
    }
    return f(xs,ys) && f(ys,xs);
}

function recordCopy<T extends {}>(xs: T): T {
    return <T>mapCopy(<MapString<any>>xs);
}

function mapCopy<V>(xs: MapString<V>): MapString<V>
{
    const res: MapString<any> = {};
    for (var s in xs)
        res[s] = xs[s];
    return res;
}

function mapUnion<V>(xs: MapString<V>, ys: MapString<V>): MapString<V>
{
    const res = mapCopy(ys);
    for (const s in xs)
        res[s] = xs[s];
    return res;
}

function concatNub<T extends key>(xss : T[][]) : T[]
{
    const res : T[] = [];
    const seen: {} = {};
    for (const xs of xss)
    {
        for (const x of xs)
        {
            const v: key = x;
            if (!(v in seen))
            {
                (<any>seen)[v] = null;
                res.push(x);
            }
        }
    }
    return res;
}
