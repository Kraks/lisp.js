#! /usr/bin/env node
/* Author: Guannan Wei
 */

var lisp = require('./lib/lisp.js');
var eval = lisp.eval;
var parse = lisp.parse;
var readline = require('readline');

var rl = readline.createInterface(process.stdin, process.stdout);
rl.setPrompt("> ");
rl.prompt();
rl.on('line', function(line) {
    var res = eval(parse(line.trim()));
    if (res) console.log(to_string(res));
    rl.prompt();
}).on('close', function() {
    console.log('Bye');
    process.exit(0);
});

