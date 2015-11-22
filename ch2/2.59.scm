(load "../lib/util.scm")

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; Solution
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (cons (car set1) set2)))))

; Tests
(run-tests-header)

(test "union-set")
(should-be (union-set '() '()) '())
(should-be (union-set '(1) '()) '(1))
(should-be (union-set '() '(1)) '(1))
