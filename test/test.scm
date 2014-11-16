(car (list 1 2 3))

(cdr (list 1 2 3))

(cons 1 2)

(cons 1 (list 2 3))

(length (list 5 5 5))

(< 1 2)

(<= 1 2)

(= 2 2)

(list? (list 1 2 3))

(list? (quote abc))

(null? (list))

(define fact
  (lambda (n)
    (if (<= n 1) 1
        (* n (fact (- n 1))))))

(fact 10)

(define count
  (lambda (item L)
    (if (not (null? L))
        (+ (equal? item (car L)) (count item (cdr L)))
        0)))

(count 2 (list 0 1 2 2 1 3))

(define add
  (lambda (x y) (+ x y)))

(add 1 2)

(let ((x 5) (y 2)) (+ x y))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f) (le (lambda (x) ((f f) x)))))))

(define FactY
  (Y (lambda (f)
       (lambda (n)
         (cond ((eq? n 0) 1)
               (else (* n (f (- n 1)))))))))

(FactY 10)

(define U
  (lambda (F) (F F)))

(define FactU
  (U (lambda (f)
       (lambda (n)
         (if (<= n 0) 1
             (* n ((U f) (- n 1))))))))

(FactU 10)
