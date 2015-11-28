(load "../lib/util.scm")

; Solution
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((= (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) (cdr s2))))
        ((< (car s1) (car s2)) (cons (car s1) (union-set (cdr s1) s2)))
        (else (cons (car s2) (union-set s1 (cdr s2))))))

; Tests
(run-tests-header)

(test "adjoin-set")
(should-be (union-set '(1 2 3) '(2 3 4)) '(1 2 3 4))
(should-be (union-set '() '(2 3 4)) '(2 3 4))
(should-be (union-set '(1 2) '()) '(1 2))
(should-be (union-set '() '()) '())
(should-be (union-set '(1 1 1) '(1 1 1)) '(1 1 1))
