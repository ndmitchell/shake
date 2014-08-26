/*jsl:option explicit*/
/*jsl:import shake-util.js*/
"use strict";

// Data
//     {name :: String
//     ,values :: [Progress]
//     }
//
// Progress
//     {idealSecs :: Double
//     ,idealPerc :: Double
//     ,actualSecs :: Double
//     ,actualPerc :: Double
//     }

$(function(){
    $("#output").html("");
    for (var i = 0; i < shake.length; i++)
    {
        var raw = [];
        var raw2 = [];
        for (var j = 10; j < shake[i].values.length; j++)
        {
            raw.push([-shake[i].values[j].idealSecs, shake[i].values[j].actualSecs]);
            raw2.push([-shake[i].values[j].idealSecs, shake[i].values[j].idealSecs]);
        }
        var ys = [{label:shake[i].name, data:raw, color:"red"},{data:raw2, color:"blue"}];
        var div = $("<div style='float:left;'>").width(300).height(200);
        $("#output").append(div);

        $.plot(div, ys, {width: 100, height: 100});
    }
})
