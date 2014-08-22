(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

; Solution
(define (cdr z)
  (z (lambda (p q) q)))

; verifying using substitution model
(define v (cons X Y))
(define v (lambda (m) (m X Y)))

(car v)
(v (lambda (p q) p))
((lambda (m) (m X Y)) (lambda (p q) p))
((lambda (p q) p) X Y)
X ; (car v) == X

; Tests
(define x (cons 3 4))

(newline)
(display "3->")(display (car x))

(newline)
(display "4->")(display (cdr x))
