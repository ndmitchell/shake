/* tslint:disable */
"use strict";

function t(a: seconds, b: seconds, c: string): Trace { return { start: a, stop: b, command: c }; }

var raw1 : ProfileRaw[] =
    // Haskell depends on Functional, C and Cpp depend on Imperative
    // Exe depends on Haskell/C/Cpp
    [["Functional", 1,  0, 3, [], [["gen",0,1]]]
    ,["Imperative", 2,  0, 0, [], [["gen",0,1],["gen",1,2]]]
    ,["HsSource",   0,  3, 3, [], []]
    ,["Haskell",    8,  3, 3, [0,2], [["ghc",1,8.9]]]
    ,["C",          15, 0, 0, [1], [["gcc",2,16.9]]]
    ,["Cpp",        10, 0, 0, [1], [["gcc",2,10]]]
    ,["Exe",        5,  0, 0, [3,4,5], [["link",17,22]]]
    ];

var dat1 : Profile[] = raw1.map(unrawProfile);


function test() : string
{
    function assert(b: boolean) : void
    {
        if (!b) throw "Assertion failed";
    }

    function assertEq<T>(got: T, want: T): void
    {
        if (want != got)
        {
            console.log("Wanted: " + want);
            console.log("Got: " + got);
            assert(false);
        }
    }

    function assertRegex(want: RegExp, got: string)
    {
        if (!want.test(got))
        {
            console.log("Wanted: " + want);
            console.log("Got: " + got);
            assert(false);
        }
    }

    var tab1 = prepare(dat1);
    profile = dat1;
    var ssum1 = reportSummary(fullSearch()).innerText;
    console.log(ssum1);
    var want = ["4 runs" ,"7 rules","5 rebuilt","7 traced","6 in","build time is 41.00s","38.80s is traced"
               ,"longest rule takes 15.00s","longest traced command takes 14.90s","parallelism of 1.40","22.00s"];
    assertRegex(new RegExp(want.join(".*")),ssum1);

    var par1 = commandPlot(tab1,"group('x')",10)['x'];
    console.log(par1);
    var pars1 = par1.items.map(function(i){return Math.round(i*10)/10;});
    assert(listEq(pars1, [1.5,2,2,2,1.5,1,1,1,1,1]));

    function chk(f : (data:Prepare, query:String) => any[], query : string, n:int)
    {
        var ans = f(tab1,query);
        console_table(ans);
        assertEq(ans.length,n);
    }

    chk(ruleTable,"",7);
    chk(ruleTable,"leaf()",3);
    chk(ruleTable,"named(/^(.)/)",5);
    chk(commandTable,"",4);
    chk(commandTable,"command(/g(.*)/)",3);
    chk(ruleTable,"childOf('Imperative')",2);

    return "passed";
}

function console_table(xs: MapString<any>[]): void
{
    // Could call console.table, but that doesn't print anything through 'node'
    if (xs.length === 0)
        console.log("No data");
    else
    {
        var widths : number[] = [];
        var cells: string[][] = [];
        for (let i = 0; i <= xs.length; i++)
            cells.push([]);
        for (const s in xs[0])
        {
            var len = s.length;
            cells[0].push(s);
            for (var i = 0; i < xs.length; i++)
            {
                var ss = "" + xs[i][s];
                len = Math.max(len, ss.length);
                cells[i+1].push(ss);
            }
            widths.push(len);
        }
        var s = "";
        for (var x = 0; x < cells.length; x++)
        {
            for (var y = 0; y < widths.length; y++)
                s += "|" + pad(widths[y], cells[x][y]);
            s += "|\n";
        }
        console.log(s);
    }
}

function pad(n: int, s: string) : string
{
    var res = s;
    for (var i = s.length; i < n; i++)
        res += " ";
    return res;
}
