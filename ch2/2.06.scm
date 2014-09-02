(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Solution
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (f2) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f (((lambda (x2) x2) x)))))
(lambda (f) (lambda (x) (f x)))
;; ->
(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))
;; ->
(define two (lambda (f) (lambda (x) (f (f x)))))

(define sum
  (lambda (m n) (lambda (f) (lambda (x) ((m f) ((n f) x))))))

; Tests
(define f (lambda (x) (+ x 1)))

(newline)
(display "0 -> ")(display ((zero f) 0))
(newline)
(display "1 -> ")(display ((one f) 0))
(newline)
(display "2 -> ")(display ((two f) 0))

(newline)
(display "1 + 2 -> ")(display (((sum one two) f) 0))
