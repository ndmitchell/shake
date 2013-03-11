/*jsl:option explicit*/
"use strict";

/////////////////////////////////////////////////////////////////////
// JQUERY EXTENSIONS

jQuery.fn.enable = function (x)
{
    // Set the values to enabled/disabled
    return this.each(function () {
        if (x)
            $(this).removeAttr('disabled');
        else
            $(this).attr('disabled','disabled');
    });
};

jQuery.getParameters = function()
{
    // From http://stackoverflow.com/questions/901115/get-querystring-values-with-jquery/3867610#3867610
    var params = {},
        e,
        a = /\+/g,  // Regex for replacing addition symbol with a space
        r = /([^&=]+)=?([^&]*)/g,
        d = function (s) { return decodeURIComponent(s.replace(a, " ")); },
        q = window.location.search.substring(1);

    while (e = r.exec(q))
        params[d(e[1])] = d(e[2]);
    return params;
};


/////////////////////////////////////////////////////////////////////
// STRING FORMATTING

function /* export */ showTime(x) // :: Double -> String
{
    function digits(x){var s = String(x); return s.length === 1 ? "0" + s : s;}

    if (x >= 3600)
    {
        var x = Math.round(x / 60);
        return Math.floor(x / 60) + "h" + digits(x % 60) + "m";
    }
    else if (x >= 60)
    {
        var x = Math.round(x);
        return Math.floor(x / 60) + "m" + digits(x % 60) + "s";
    }
    else
        return x.toFixed(2) + "s";
}

function /* export */ showPerc(x) // :: Double -> String
{
    return (x*100).toFixed(2) + "%";
}

function plural(n,not1,is1) // :: Int -> Maybe String -> Maybe String -> String
{
    return n === 1
        ? (is1 === undefined ? "" : is1)
        : (not1 === undefined ? "s" : not1);
}


/////////////////////////////////////////////////////////////////////
// MISC

function sum(xs) // :: Num a => [a] -> a
{
    var res = 0;
    for (var i = 0; i < xs.length; i++)
        res += xs[i];
    return res;
}

function testRegExp(r, s)
{
    if (typeof r === "string")
        return s.indexOf(r) !== -1;
    else
        return r.test(s);
}

function execRegExp(r, s)
{
    if (typeof r === "string")
        return s.indexOf(r) === -1 ? null : [];
    else
        return r.exec(s);
}

function listEq(xs, ys) // :: Eq a => [a] -> [a] -> Bool
{
    if (xs.length !== ys.length) return false;
    for (var i = 0; i < xs.length; i++)
    {
        if (xs[i] !== ys[i])
            return false;
    }
    return true;
}

function cache(str, f) // :: (k -> String) -> (k -> v) -> (k -> v)
{
    var cache = {};
    return function(k){
        var s = str(k);
        if (!(s in cache))
            cache[s] = f(k);
        return cache[s];
    }
}

function recordEq(xs, ys) // :: Record -> Record -> Bool
{
    function f(a,b)
    {
        for (var s in a)
        {
            if (a[s] !== b[s]) return false;
        }
        return true;
    }
    return f(xs,ys) && f(ys,xs);
}

function recordCopy(xs) // :: Record -> Record
{
    var res = {};
    for (var s in xs)
        res[s] = xs[s];
    return res;
}

function recordUnion(xs,ys) // :: Record -> Record -> Record -- left biased
{
    var res = recordCopy(ys);
    for (var s in xs)
        res[s] = xs[s];
    return res;
}

function concatNub(xs) // :: Eq a => [[a]] -> [a]
{
    var res = [];
    var seen = {};
    for (var i = 0; i < xs.length; i++)
    {
        var x = xs[i];
        for (var j = 0; j < x.length; j++)
        {
            var e = x[j];
            if (!(e in seen))
            {
                seen[e] = null;
                res.push(e);
            }
        }
    }
    return res;
}
