var _ = require('./underscore');

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
    var f_template = function(g) {
        return function() { return _.reduce(Array.prototype.slice.apply(arguments), g); };
    };
    env.update({
        "+":       f_template(function(x, y) { return x+y; }),
        "-":       f_template(function(x, y) { return x-y; }),
        "*":       f_template(function(x, y) { return x*y; }),
        "/":       f_template(function(x, y) { return x/y; }),
        "not":     function(val)  { return !val; },
        "and":     function(x, y) { return x && y; },
        "or":      function(x, y) { return x || y; },
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

var global_env = add_globals(new Env());

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
        var end = x.prototype.slice(-1);
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

var Symbol = function(s) { return s.toString(); };

var read = function(s) { return read_from(tokenize(s)); };

var parse = read;

var tokenize = function(s) {
    return _.filter(s.replace(/\(/g, " ( ").replace(/\)/g, " ) ").split(" "),
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
            return Symbol(token);
        }
        return parseInt(token);
    }
    return parseFloat(token);
};

var to_string = function(exp) {
    if (isa(exp, "list")) {
        return "(" + exp.map(to_string).join(" ") + ")";    
    }
    return exp.toString();
};

console.log(eval(parse("(+ 1 2 3)")))
console.log(eval(parse("(* 2 3 4)")))

console.log(eval(parse("(car (list 1 2 3))")));
console.log(eval(parse("(cdr (list 1 2 3))")));
console.log(eval(parse("(cons 1 2)")));
console.log(eval(parse("(cons 1 (list 2 3))")));
console.log(eval(parse("(length (list 9 9 9))")));
console.log(eval(parse("(< 1 2)")));
console.log(eval(parse("(<= 1 2)")));
console.log(eval(parse("(= 2 2)")));
console.log(eval(parse("(list? (list 1 2 3))")));
console.log(eval(parse("(null? (list))"))); 

eval(parse("(define add (lambda (x y) (+ x y)))"));
console.log(eval(parse("(add 2 4)")));

eval(parse("(define area (lambda (r) (* 3.141592653 (* r r))))"));
console.log(eval(parse("(area 3)")));

eval(parse("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"));
console.log(eval(parse("(fact 10)")));
console.log(eval(parse("(if (equal? 1 1) (quote conseq) (quote otherwise))")));
console.log(eval(parse("(if (equal? 1 2) (quote conseq) (quote otherwise))")));
console.log(eval(parse("(not (equal? 1 1))")));

eval(parse("(define first car)"));
eval(parse("(define rest cdr)"));

console.log(eval(parse("(first (list 1 2 3))")));
console.log(eval(parse("(rest (list 1 2 3))")));
console.log(eval(parse("(equal? 1 (first (list 1 2 3)))")));

eval(parse("(define count (lambda (item L) (if (not (null? L)) (+ (equal? item (first L)) (count item (rest L))) 0)))"));
console.log(eval(parse("(count 2 (list 0 1 2 2 0 2))"))); //3

console.log(eval(parse("(count 0 (list 0 1 2 3 0 2))"))); //2

console.log(eval(parse("(and (equal? 1 2) (equal? 2 2))")));
console.log(eval(parse("(or (equal? 1 2) (equal? 2 2))")));

console.log(eval(parse("(let ((x 5)) x)")));
console.log(eval(parse("(let ((x 5) (y 2)) (+ x y))"))); //7