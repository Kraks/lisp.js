var _ = require('./underscore');

var Env = function(params, args, outer) {
    var dict = {};
    var that = this;
    var outer = outer;
    _.extend(dict, _.object(params, args));
    this.find = function(name) {
        //console.log(name);
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
        "and":     f_template(function(x, y) { return x && y; }),
        "or":      f_template(function(x, y) { return x || y; }),
        "else":    true,
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

var Symbol = function(s) { return s.toString(); };

var read = function(s) { return read_from(tokenize(s)); };

var parse = read;

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

console.log(eval(parse("(+ 1 2 3)"))) //6
console.log(eval(parse("(* 2 3 4)"))) //24

console.log(eval(parse("(car (list 1 2 3))"))); //1
console.log(eval(parse("(cdr (list 1 2 3))"))); //[2, 3]
console.log(eval(parse("(cons 1 2)"))); //[1, 2]
console.log(eval(parse("(cons 1 (list 2 3))"))); //[1, 2, 3]
console.log(eval(parse("(length (list 9 9 9))"))); //3
console.log(eval(parse("(< 1 2)"))); //true
console.log(eval(parse("(<= 1 2)"))); // true
console.log(eval(parse("(= 2 2)"))); //true
console.log(eval(parse("(list? (list 1 2 3))"))); //true
console.log(eval(parse("(list? (quote abc))"))); //false
console.log(eval(parse("(null? (list))"))); //true

eval(parse("(define add (lambda (x y) (+ x y)))")); 
console.log(eval(parse("(add 2 4)"))); //6

eval(parse("(define area (lambda (r) (* 3.141592653 (* r r))))"));
console.log(eval(parse("(area 3)"))); //28.2xxxxx

eval(parse("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"));
console.log(eval(parse("(fact 10)"))); //3628800
console.log(eval(parse("(if (equal? 1 1) (quote conseq) (quote otherwise))"))); //conseq
console.log(eval(parse("(if (equal? 1 2) (quote conseq) (quote otherwise))"))); //otherwise
console.log(eval(parse("(not (equal? 1 1))"))); // false

eval(parse("(define first car)"));
eval(parse("(define rest cdr)"));

console.log(eval(parse("(first (list 1 2 3))"))); //1
console.log(eval(parse("(rest (list 1 2 3))"))); //[2, 3]
console.log(eval(parse("(equal? 1 (first (list 1 2 3)))"))); //true

eval(parse("(define count (lambda (item L) (if (not (null? L)) (+ (equal? item (first L)) (count item (rest L))) 0)))"));
console.log(eval(parse("(count 2 (list 0 1 2 2 0 2))"))); //3

console.log(eval(parse("(count 0 (list 0 1 2 3 0 2))"))); //2

console.log(eval(parse("(and (equal? 1 2) (equal? 2 2))"))); //false
console.log(eval(parse("(or (equal? 1 2) (equal? 2 2))"))); //true

console.log(eval(parse("(cond ((> (area 3) 30) (quote should_not_happen)) ((equal? 1 2) (quote should_not_happen)) (else (quote else_happen)))")));
console.log(eval(parse("(let ((x 5)) x)"))); //5
console.log(eval(parse("(let ((x 5) (y 2)) (+ x y))"))); //7
console.log(eval(parse("(let* ((x 5) (y (+ x 2))) (+ x y))"))); //12

eval(parse("(define Y (lambda (le) ((lambda (f) (f f)) (lambda (f) (le (lambda (x) ((f f) x)))))))"));
eval(parse("(define FactY (Y (lambda (f) (lambda (n) (cond ((eq? n 0) 1) (else (* n (f (- n 1)))))))))"));
console.log(eval(parse("(FactY 10)"))); //3628800