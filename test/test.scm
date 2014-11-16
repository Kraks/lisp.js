(car (list 1 2 3))

(cdr (list 1 2 3))

(cons 1 2)

(cons 1 (list 2 3))

(length (list 5 5 5))

(define add
  (lambda (x y) (+ x y)))

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
