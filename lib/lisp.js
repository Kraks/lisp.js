/* Author: Guannan Wei <kiss.kraks@gmail.com>
 */

var _ = require('underscore');

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
    var r_template = function(g) {
        return function() { return _.reduce(Array.prototype.slice.apply(arguments), g); };
    };
    env.update({
        "else":    true,
        "#t":      true,
        "#f":      false,
        "+":       r_template(function(x, y) { return x + y; }),
        "-":       r_template(function(x, y) { return x - y; }),
        "*":       r_template(function(x, y) { return x * y; }),
        "/":       r_template(function(x, y) { return x / y; }),
        "and":     r_template(function(x, y) { return x && y; }),
        "or":      r_template(function(x, y) { return x || y; }),
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

    if (isa(x, "Symbol")) { return env.find(x).get(x); }
    else if (!isa(x, "list")) { return x; }
    else if (x[0] === "quote") { return x[1]; }
    else if (x[0] === "if") {
        if (eval(x[1], env)) { return eval(x[2], env); }
        else { return eval(x[3], env); }
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
        return eval(_.find(x.slice(1), function(item) { return eval(item[0], env); })[1], env);
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
};

var parse = function(s) { return read_from(tokenize(s)); };

var tokenize = function(s) {
    return _.filter(s.replace(/[\r\t\n]/g, " ").replace(/\(/g, " ( ").replace(/\)/g, " ) ").split(" "),
             function(item) { return !_.isEmpty(item); });
};

var read_from = function(tokens) {
    if (tokens.length === 0)
        throw new Error("Syntax Error: unexpected EOF while reading");
    token = tokens.shift();
    if ("(" === token) {
        var t = [];
        while (tokens[0] != ")") {
            t.push(read_from(tokens));
        }
        tokens.shift();
        return t;
    }
    else if (")" === token) {
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

var toString = function(exp) {
    if (isa(exp, "list")) {
        return "(" + exp.map(toString).join(" ") + ")";    
    }
    return exp.toString();
};

var global_env = add_globals(new Env());

exports.eval = eval;
exports.parse = parse;
exports.toString = toString;
