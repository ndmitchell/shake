// RUN TEST USING:
// pushd ts && tsc && popd && node test.js

var fs = require("fs");
var vm = require("vm");

function include(path) {
    var code = fs.readFileSync(path, 'utf-8');
    vm.runInThisContext(code, global);
}

// Stub enough things to ignore UI pieces

class HTMLElement {
    constructor (str) {
        this.innerText = str;
    }
    appendChild(x) {
        this.innerText += x.innerText;
    }
    setAttribute() {}
}

function createElement(typ) {
    return new HTMLElement("");
}

function createTextNode(str) {
    return new HTMLElement(str);
}

global.window = {};
global.jQuery = {fn : {}};
global.document = {
    createElement: createElement,
    createTextNode: createTextNode
};

include("data/profile-data.js");
include("data/version.js");
include("shake.js");
test();
