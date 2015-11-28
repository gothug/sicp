(load "../lib/util.scm")

; Solution
(define (adjoin-set x set)
  (if (null? set)
      (list x)
      (let ((h (car set))
            (t (cdr set)))
            (cond ((= x h) set)
                  ((> x h) (cons h (adjoin-set x t)))
                  (else (cons x set))))))

; Tests
(run-tests-header)

(test "adjoin-set")
(should-be (adjoin-set 1 '()) '(1))
(should-be (adjoin-set 0 '(1 3 5)) '(0 1 3 5))
(should-be (adjoin-set 4 '(1 3 5)) '(1 3 4 5))
(should-be (adjoin-set 6 '(1 3 5)) '(1 3 5 6))
