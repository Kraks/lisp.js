/**
 * CPS transformation 
 */

var _ = require('underscore');
var parse = require('./parser.js').parse;

var currying = function(ast) {
    if (ast[0] == "lambda") {
        if (ast[1].length > 1) {
            var res = [];
            res.push("lambda", [ast[1][0]]);
            res.push(currying(["lambda", ast[1].slice(1), ast[2]]));
            return res;
        }
        return ast;
    }
    else {
        //apply
        return _.reduce(ast, function(x, y) {
            if (typeof x === 'object') {
                x = currying(x);
            }
            if (typeof y === 'object') {
                y = currying(y);
            }
            return [x, y]; 
        });
    }
};

var cps_convert = function(term, cont) {
    
};
