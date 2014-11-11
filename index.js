#! /usr/bin/env node
/* Author: Guannan Wei
 */

var _ = require('underscore');
var readline = require('readline');
var lisp = require('./lib/lisp.js');
var eval = lisp.eval;
var parse = lisp.parse;
var toString = lisp.toString;

var rl = readline.createInterface(process.stdin, process.stdout);
rl.setPrompt("> ");
rl.prompt();
rl.on('line', function(line) {
    var res = eval(parse(line.trim()));
    if (!_.isUndefined(res))
        console.log(toString(res));
    rl.prompt();
}).on('close', function() {
    console.log('Bye');
    process.exit(0);
});
