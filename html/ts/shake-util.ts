/*jsl:option explicit*/
"use strict";

type key = string | number | symbol;

type seconds = number

type int = number


/////////////////////////////////////////////////////////////////////
// JQUERY EXTENSIONS

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

var getParameters = function()
{
    // From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    var params = {};
    var a = /\+/g;  // Regex for replacing addition symbol with a space
    var r = /([^&=]+)=?([^&]*)/g;
    var d = function (s) { return decodeURIComponent(s.replace(a, " ")); };
    var q = window.location.search.substring(1);

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
    function digits(x){var s = String(x); return s.length === 1 ? "0" + s : s;}

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
    var res = 0;
    for (var i = 0; i < xs.length; i++)
        res += xs[i];
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
    for (var i = 0; i < xs.length; i++)
    {
        if (xs[i] !== ys[i])
            return false;
    }
    return true;
}

function cache<K,V>(key : (k:K) => key, op : (k:K) => V) : (k:K) => V
{
    var cache = {};
    return function(k){
        var s = key(k);
        if (!(s in cache))
            cache[s] = op(k);
        return cache[s];
    };
}

function recordEq<T extends {}>(xs: T, ys: T) : boolean
{
    function f(a : T, b : T)
    {
        for (var s in a)
        {
            if (a[s] !== b[s]) return false;
        }
        return true;
    }
    return f(xs,ys) && f(ys,xs);
}

function recordCopy<T extends {}>(xs: T): T
{
    var res = {};
    for (var s in xs)
        res[s] = xs[s];
    return <T>res;
}

function recordUnion<T,U>(xs : T,ys : U) : T&U
{
    var res = recordCopy(ys);
    for (var s in xs)
        res[s] = xs[s];
    return <T&U>res;
}

function concatNub<T extends key>(xs : T[][]) : T[]
{
    var res : T[] = [];
    var seen = {};
    for (var i = 0; i < xs.length; i++)
    {
        var x = xs[i];
        for (var j = 0; j < x.length; j++)
        {
            var e = x[j];
            var ee: key = e;
            if (!(ee in seen))
            {
                seen[ee] = null;
                res.push(e);
            }
        }
    }
    return res;
}
