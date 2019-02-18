// RUN TEST USING:
// pushd ts && tsc && popd && node test.js

var fs = require("fs");
var vm = require("vm");

function include(path) {
    var code = fs.readFileSync(path, 'utf-8');
    vm.runInThisContext(code, global);
}

// Stub enough things to ignore UI pieces

global.window = {};
global.jQuery = {fn : {}};

include("data/profile-data.js");
include("shake.js");
test();
