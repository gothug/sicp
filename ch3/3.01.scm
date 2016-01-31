(load "../lib/util.scm")

; Solution
(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
           sum))

; Tests
(run-tests-header)

(test "make-accumulator")
(define A (make-accumulator 5))
(should-be (A 10) 15)
(should-be (A 10) 25)
