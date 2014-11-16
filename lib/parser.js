/** Lisp.js
 *  Parser
 */

var LPAREN = '(';
var RPAREN = ')';

var _ = require('underscore');

var parse = function(s) { return read_from(tokenize(s)); };

var tokenize = function(s) {
    return _.filter(s.replace(/[\r\t\n]/g, " ").replace(/\(/g, " ( ").replace(/\)/g, " ) ").split(" "),
                    function(item) { return !_.isEmpty(item); });
};

var read_from = function(tokens) {
    if (tokens.length === 0)
        throw new Error("Syntax Error: unexpected EOF while reading");
    token = tokens.shift();
    if (LPAREN === token) {
        var t = [];
        while (tokens[0] != RPAREN) {
            t.push(read_from(tokens));
        }
        tokens.shift();
        return t;
    }
    else if (RPAREN === token) {
        throw new Error("Syntax Error: unexpected )");
    }
    else {
        return atom(token);
    }
};

var atom = function(token) {
    if (isNaN(parseFloat(token))) {
        if (isNaN(parseInt(token))) {
            return token.toString();
        }
        return parseInt(token);
    }
    return parseFloat(token);
};

var read_expr_pos = function(string) {
    var i = 0;

    if (string[0] != LPAREN)
        return i;
    for (pos in string) {
        if (string[pos] === LPAREN)
            i++;
        if (string[pos] === RPAREN)
            i--;
        if (i == 0)
            return parseInt(pos)+1;
    }
    return parseInt(pos);
};

exports.parse = parse;
exports.read_expr_pos = read_expr_pos;
