// first find which runs have built data in them, sorted
// built :: [Int]
var built = function(){
  var seen = {};
  for (var i = 0; i < shake.length; i++)
    seen[shake[i].built] = true;
  var seen2 = [];
  for (var i in seen)
    seen2.push(i);
  return seen2.sort(function cmp(a,b){return a-b;});
}();

// what is the index of the last build run
// lastRun :: Int
var lastRun = built.length == 0 ? 0 : built[built.length-1];

function showTime(x){return x.toFixed(2) + "s";}
function showPerc(x){return (x*100).toFixed(2) + "%";}
function showShort(x){return x;}

function plural(n,not1,is1){
  return n == 1
    ? (is1 == undefined ? "" : is1)
    : (not1 == undefined ? "s" : not1);
}


function listEq(xs, ys)
{
  if (xs.length != ys.length) return false;
  for (var i = 0; i < xs.length; i++)
    if (xs[i] != ys[i]) return false;
  return true;
}

// Return the n top entries, grouping as necessary by words
// huffman :: Int -> [{sum :: Double, name :: String}] -> [{sum :: Double, count :: Int, name :: String}]
function huffman(resultSize, xs)
{
  var val = {sum: 0, count: 0, children: {}};

  for (var i = 0; i < xs.length; i++)
  {
    var val2 = val;
    var ys = xs[i].name.split(' ');
    for (var j = 0; j < ys.length; j++)
    {
      if (val2.children[ys[j]])
        val2 = val2.children[ys[j]];
      else
      {
        var t = {sum: 0, count: 0, children: {}};
        val2.children[ys[j]] = t;
        val2 = t;
      }
      val2.sum += xs[i].sum;
      val2.count++;
    }
  }

  function compress(val) // return the number of items
  {
    var n = 0;
    for (var j in val.children)
      n += compress(val.children[j]);
    if (n == 1)
      val.children = {};
    return Math.max(1,n);
  }
  compress(val);

  function flatten(path, val, res)
  {
    for (var i in val.children)
    {
      var ii = val.children[i];
      var pp = path.slice(0);
      pp.push(i);
      res.push({sum:ii.sum, count:ii.count, name:pp, free:path.length==0});
      flatten(pp,ii,res);
    }
  }
  var flat = [];
  flatten([], val, flat);

  while(true)
  {
    // order everything
    flat.sort(function(a,b){var i = b.sum - a.sum; return i != 0 ? i : b.count - a.count;});

    // now, if something in the first n is not free, mark it free and delete anyone who relies on it
    // then repeat, if nothing is not free, break
    var cont = false;
    for (var i = 0; i < Math.min(resultSize, flat.length); i++)
    {
      if (!flat[i].free)
      {
        cont = true;
        flat[i].free = true;
        for (var j = 0; j < flat.length; j++)
        {
          if (listEq(flat[i].name.slice(0,flat[i].name.length-1), flat[j].name))
          {
            flat[j].count -= flat[i].count;
            flat[j].sum -= flat[i].sum;
          }
        }
        break;
      }
    }
    if (!cont) break;
  }

  for (var i = 0; i < flat.length; i++)
    flat[i].name = flat[i].name.join(" ");
  return flat.slice(0, resultSize);
}


function load()
{
  /////////////////////////////////////////////////////////////////
  // SUMMARY INFORMATION

  var countLast = 0;
  var sumExecution = 0;
  var maxExecution = 0;
  var countTrace = 0, countTraceLast = 0;
  var sumTrace = 0;
  var maxTrace = 0;
  var maxTraceStopLast = 0;
  for (var i = 0; i < shake.length; i++)
  {
    var isLast = shake[i].built == lastRun;
    countLast += isLast ? 1 : 0;
    sumExecution += shake[i].execution;
    maxExecution = Math.max(maxExecution, shake[i].execution);
    var traces = shake[i].traces;
    if (!traces) continue;
    for (var j = 0; j < traces.length; j++)
    {
      countTrace += 1;
      countTraceLast += isLast ? 1 : 0;
      sumTrace += traces[j].stop - traces[j].start;
      maxTrace = Math.max(maxTrace, traces[j].stop - traces[j].start);
      maxTraceStopLast = Math.max(maxTraceStopLast, isLast ? traces[j].stop : 0);
    }
  }

  var summary =
    "<ul>" +
    "<li><strong>Runs:</strong> This database has tracked " + built.length + " run" + plural(built.length) + ".</li>" +
    "<li><strong>Rules:</strong> There are " + shake.length + " rules (" + countLast + " rebuilt in the last run).</li>" +
    "<li><strong>Commands:</strong> Building required " + countTrace + " traced commands (" + countTraceLast + " in the last run).</li>" +
    "<li><strong>Build time:</strong> The total (unparallelised) build time is " + showTime(sumExecution) + " of which " + showTime(sumTrace) + " is traced commands.</li>" +
    "<li><strong>Longest steps:</strong> The longest rule takes " + showTime(maxExecution) + ", and the longest traced command takes " + showTime(maxTrace) + ".</li>" +
    "<li><strong>Parallelism:</strong> Last run gave an average parallelism of " + (sumTrace / maxTraceStopLast).toFixed(2) + " times over " + showTime(maxTraceStopLast) + ".</li>" +
    "</ul>";
  $('#summary').append(summary);


  /////////////////////////////////////////////////////////////////
  // PARALLELISM GRAPH

  var buckets = [];
  var countBuckets = 100;
  for (var i = 0; i <= countBuckets; i++)
    buckets.push(0); // fill with 1 more element, but the last bucket will always be 0

  for (var i = 0; i < shake.length; i++)
  {
    var traces = shake[i].traces;
    if (!traces || shake[i].built != lastRun) continue;
    for (var j = 0; j < traces.length; j++)
    {
      var start = traces[j].start * countBuckets / maxTraceStopLast;
      var stop = traces[j].stop * countBuckets / maxTraceStopLast;

      if (Math.floor(start) == Math.floor(stop))
        buckets[Math.floor(start)] += stop - start;
      else
      {
        for (var k = Math.ceil(start); k < Math.floor(stop); k++)
          buckets[k]++;
        buckets[Math.floor(start)] += Math.ceil(start) - start;
        buckets[Math.floor(stop)] += stop - Math.floor(stop);
      }
    }
  }
  var maxBucket = 0;
  for (var i = 0; i < countBuckets; i++)
    maxBucket = Math.max(maxBucket, buckets[i]);
  maxBucket = Math.ceil(maxBucket - 0.00001);

  plotvals = [];
  for (var i = 0; i < countBuckets; i++)
    plotvals.push([i, (buckets[i] * 100 / maxBucket)]);
  $.plot($('#shakeplot'), [plotvals]);

  /////////////////////////////////////////////////////////////////
  // MOST EXPENSIVE RULES

  var top = shake.slice(0).sort(function(a,b){return b.execution-a.execution}).slice(0,15);
  var rules = "<tbody>";
  for (var i = 0; i < top.length; i++)
  {
    rules += "<tr>" +
      "<td><div class='progress progress-success' style='height: 10px'>" +
      "<div class='bar' style='width:" + (top[i].execution * 40 / top[0].execution) + "px;'></div></div></td>" +
      "<td>" + showTime(top[i].execution) + "</td>" +
      "<td>" + showPerc(top[i].execution / sumExecution) + "</td>" +
      "<td>" + top[i].name + "</td>" +
      "</tr>";
  }
  rules += "</tbody>";
  $('#rule-details').append(rules);

  /////////////////////////////////////////////////////////////////
  // MOST EXPENSIVE COMMANDS

  var tooList = [];
  for (var i = 0; i < shake.length; i++)
  {
    var traces = shake[i].traces;
    if (!traces) continue;
    for (var j = 0; j < traces.length; j++)
      tooList.push({name: traces[j].command, sum: traces[j].stop - traces[j].start});
  }

  toolList = huffman(15, tooList);
  var commands = "<tbody>";
  for (var i = 0; i < toolList.length; i++)
  {
    commands += "<tr>" +
      "<td><div class='progress progress-success' style='height: 10px'>" +
      "<div class='bar' style='width:" + (toolList[i].sum * 40 / toolList[0].sum) + "px;'></div></div></td>" +
      "<td>" + showTime(toolList[i].sum) + "</td>" +
      "<td>" + showPerc(toolList[i].sum / sumExecution) + "</td>" +
      "<td>" + toolList[i].count + "&nbsp;&times;</td>" +
      "<td>" + toolList[i].name + "</td>" +
      "</tr>";
  }
  commands += "</tbody>";
  $('#cmd-details').append(commands);
}
