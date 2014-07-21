
function convertMakefile(s) // :: Makefile -> Shakefile
{
    return s;
}

function convertNinja(s) // :: Ninja -> Shakefile
{
    return s;
}

function convertShake(s) // :: Shakefile -> Shake
{
    var res = [];
    var src = s.split("\n");
    res.push("import Development.Shake");
    for (var i = 0; i < src.length; i++)
    {
        var x = src[i];
        if (x.substr(0,8) === "include ")
            res.push("import " + x);
    }
    res.push("");
    res.push("main = shakeWithArgs shakeOptions $ do")
    for (var i = 0; i < src.length; i++)
    {
        var x = src[i];
        if (x.substr(0,8) === "include ")
            null;
        else if (x.substr(0,1) === "#")
            res.push("    -- " + x.substr(1));
        else if (x.charAt(x.length-1) === ":")
        {
            res.push("    \"" + x.substr(0,x.length-1) + "\" *> \\out -> do");
        }
        else if (x.trimLeft().substr(0,5) === "need ")
            res.push("        need [\"" + x.trimLeft().substr(5) + "\"]");
        else if (x.trim() === "")
            res.push("");
        else
            res.push("        cmd Shell \"" + x.trim() + "\"");
    }
    return res.join("\n");
}
