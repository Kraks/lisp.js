/**
 * Lisp.js core
 * Author: Guannan Wei 
 * Contact: kiss.kraks@gmail.com
 */

var _ = require('underscore');
var parser = require('./parser');

var Env = function(params, args, outer) {
    var dict = {};
    var that = this;
    var outer = outer;
    _.extend(dict, _.object(params, args));
    this.find = function(name) {
        if (_.has(dict, name)) return that;
        return outer.find(name);
    };
    this.set = function(name, val) { dict[name] = val; };
    this.update = function(new_dict) { _.extend(dict, new_dict); };
    this.get = function(name) {
        if (_.has(dict, name)) return dict[name];
    };
};

var add_globals = function(env) {
    var combine_op = function(g) {
        return function() { return _.reduce(Array.prototype.slice.apply(arguments), g); };
    };
    env.update({
        "else":    true,
        "#t":      true,
        "#f":      false,
        "+":       combine_op(function(x, y) { return x + y; }),
        "-":       combine_op(function(x, y) { return x - y; }),
        "*":       combine_op(function(x, y) { return x * y; }),
        "/":       combine_op(function(x, y) { return x / y; }),
        "and":     combine_op(function(x, y) { return x && y; }),
        "or":      combine_op(function(x, y) { return x || y; }),
        "not":     function(val)  { return !val; },
        ">":       function(x, y) { return x > y; },
        "<":       function(x, y) { return x < y; },
        ">=":      function(x, y) { return x >= y; },
        "<=":      function(x, y) { return x <= y; },
        "=":       function(x, y) { return x === y; },
        "equal?":  function(x, y) { return x === y; },
        "eq?":     function(x, y) { return x == y; },
        "length":  function(x)    { return x.length },
        "cons":    function(x, y) { return [x].concat(y); },
        "car":     function(x)    { return x[0]; },
        "cdr":     function(x)    { return x.slice(1); },
        "append":  function(x, y) { x.push(y); return x; },
        "list":    function()     { return Array.prototype.slice.apply(arguments); },
        "list?":   function(x)    { return isa(x, "list"); },
        "null?":   function(x)    { return _.isEmpty(x); },
        "symbol?": function(x)    { return isa(x, "Symbol"); }
    });
    return env;
};


var eval = function(x, env) {
    if (env === void 0)
        env = global_env;

    if (isa(x, "Symbol")) { 
        return env.find(x).get(x); 
    }
    else if (!isa(x, "list")) { 
        return x; 
    }
    else if (x[0] === "quote") { 
        return x[1]; 
    }
    else if (x[0] === "if") {
        if (eval(x[1], env)) { 
            return eval(x[2], env); 
        }
        else { 
            return eval(x[3], env); 
        }
    }
    else if (x[0] === "let") {
        var o = _.object(x[1]);
        return eval([eval(["lambda", _.keys(o), x[2]], env)].concat(_.values(o)), env);
    }
    else if (x[0] === "let*") {
        var env1 = new Env(null, null, env);
        var t = _.map(_.object(x[1]), function(item, key) {
            var result = eval(item, env1);
            env1.set(key, result);
            return [key, result];
        });
        return eval(["let", t, x[2]], env);
    }
    else if (x[0] === "cond") {
        return eval(_.find(x.slice(1), function(item) { 
            return eval(item[0], env); 
        })[1], env);
    }
    else if (x[0] === "set!") {
        env.find(x[1]).set(x[1], eval(x[2], env));
    }
    else if (x[0] === "define") {
        env.set(x[1], eval(x[2], env));
    }
    else if (x[0] === "lambda") {
        return function() {
            return eval(x[2], new Env(x[1], Array.prototype.slice.apply(arguments), env));
        };
    }
    else if (x[0] === "begin") {
        var end = x.pop();
        _.each(x, function(ele) {
            eval(ele, env);
        });
        return eval(end, env);
    }
    else {
        // application
        var exps = _.map(x, function(exp) {
            return eval(exp, env);
        });
        var proc = exps.shift();
        return proc.apply(undefined, exps);
    }
};

var isa = function(s, type) {
    if (type === "list") {
        return Array.isArray(s);
    }
    else if (type === "Symbol") {
        return typeof s === "string";
    }
    else {
        return typeof s === type;
    }
};

var log = function(message) {
    if (typeof message !== 'string') {
        message = JSON.stringify(message);
    }
    console.log(message);
};

var toString = function(exp) {
    if (isa(exp, "list")) {
        return "(" + exp.map(toString).join(" ") + ")";    
    }
    return exp.toString();
};

var global_env = add_globals(new Env());

exports.eval = eval;
exports.toString = toString;
exports.parse = parser.parse;
