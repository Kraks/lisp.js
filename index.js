#! /usr/bin/env node
/** Lisp.js
 *  Author: Guannan Wei <kiss.kraks@gmail.com>
 */

var _ = require('underscore');
var fs = require('fs');
var readline = require('readline');

var lisp = require('./lib/lisp.js');
var eval = lisp.eval;
var parse = lisp.parse;
var toString = lisp.toString;
var read_expr_pos = require('./lib/parser.js').read_expr_pos;

var read_file = function(filename) {
    fs.readFile(filename, 'utf-8', function(err, data) {
        if (err) {
            console.log("Error: can not read file " + filename);
        }
        else {
            data = data.trim();
            while (data.length > 0) {
                var next_pos = read_expr_pos(data);
                var expr = data.slice(0, next_pos);
                var res = eval(parse(expr.trim()));
                if (!_.isUndefined(res))
                    console.log(toString(res));
                var data = data.slice(next_pos).trim();
            }
        }
    });
};

var interactive = function() {
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
};

if (process.argv.length > 2) {
    var file_list = process.argv.slice(2);
    _.each(file_list, read_file);
}
else {
    interactive();
}
