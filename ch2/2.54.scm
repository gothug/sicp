(load "../lib/util.scm")

; Solution
(define (equal-2? a b)
  (or
   (and
    (symbol? a)
    (symbol? b)
    (eq? a b))
   (and
    (number? a)
    (number? b)
    (= a b))
   (and
    (null? a)
    (null? b))
   (and
    (pair? a)
    (pair? b)
    (equal? (car a) (car b))
    (equal? (cdr a) (cdr b)))))

; Tests
(run-tests-header)

(should-be (equal-2? 'a 'a) #t)
(should-be (equal-2? 'a 'b) #f)

(should-be (equal-2? '(a b c) '(a b c)) #t)
(should-be (equal-2? '(a b c) '(a (b) c)) #f)

(should-be (equal-2? '() '()) #t)
(should-be (equal-2? '(a) '()) #f)

(should-be (equal-2? 25 25) #t)
(should-be (equal-2? 25 26) #f)
