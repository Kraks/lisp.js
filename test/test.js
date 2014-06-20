var lisp = require('../lib/lisp.js');
var eval = lisp.eval;
var parse = lisp.parse;

var test = function() {
    console.log(eval(parse("(+ 1 2 3)"))) // => 6
    console.log(eval(parse("(* 2 3 4)"))) // => 24

    console.log(eval(parse("(car (list 1 2 3))"))); // => 1
    console.log(eval(parse("(cdr (list 1 2 3))"))); // => [2, 3]
    console.log(eval(parse("(cons 1 2)"))); // => [1, 2]
    console.log(eval(parse("(cons 1 (list 2 3))"))); // => [1, 2, 3]
    console.log(eval(parse("(length (list 9 9 9))"))); // => 3
    console.log(eval(parse("(< 1 2)"))); // => true
    console.log(eval(parse("(<= 1 2)"))); // => true
    console.log(eval(parse("(= 2 2)"))); // => true
    console.log(eval(parse("(list? (list 1 2 3))"))); // => true
    console.log(eval(parse("(list? (quote abc))"))); // => false
    console.log(eval(parse("(null? (list))"))); // => true

    eval(parse("(define add (lambda (x y) (+ x y)))")); 
    console.log(eval(parse("(add 2 4)"))); // => 6

    eval(parse("(define area (lambda (r) (* 3.141592653 (* r r))))"));
    console.log(eval(parse("(area 3)"))); // => 28.2xxxxx

    eval(parse("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"));
    console.log(eval(parse("(fact 10)"))); // => 3628800
    console.log(eval(parse("(if (equal? 1 1) (quote conseq) (quote otherwise))"))); // => conseq
    console.log(eval(parse("(if (equal? 1 2) (quote conseq) (quote otherwise))"))); // => otherwise
    console.log(eval(parse("(not (equal? 1 1))"))); // => false

    eval(parse("(define first car)"));
    eval(parse("(define rest cdr)"));

    console.log(eval(parse("(first (list 1 2 3))"))); // => 1
    console.log(eval(parse("(rest (list 1 2 3))"))); // => [2, 3]
    console.log(eval(parse("(equal? 1 (first (list 1 2 3)))"))); // => true

    eval(parse("(define count (lambda (item L) (if (not (null? L)) (+ (equal? item (first L)) (count item (rest L))) 0)))"));
    console.log(eval(parse("(count 2 (list 0 1 2 2 0 2))"))); // => 3

    console.log(eval(parse("(count 0 (list 0 1 2 3 0 2))"))); // => 2

    console.log(eval(parse("(and (equal? 1 2) (equal? 2 2))"))); // => false
    console.log(eval(parse("(or (equal? 1 2) (equal? 2 2))"))); // => true

    console.log(eval(parse("(cond ((> (area 3) 30) (quote should_not_happen)) ((equal? 1 2) (quote should_not_happen)) (else (quote else_happen)))")));
    console.log(eval(parse("(let ((x 5)) x)"))); // => 5
    console.log(eval(parse("(let ((x 5) (y 2)) (+ x y))"))); // => 7
    console.log(eval(parse("(let* ((x 5) (y (+ x 2))) (+ x y))"))); // => 12

    eval(parse("(define Y (lambda (le) ((lambda (f) (f f)) (lambda (f) (le (lambda (x) ((f f) x)))))))"));
    eval(parse("(define FactY (Y (lambda (f) (lambda (n) (cond ((eq? n 0) 1) (else (* n (f (- n 1)))))))))"));
    console.log(eval(parse("(FactY 10)"))); // => 3628800
}

test();
