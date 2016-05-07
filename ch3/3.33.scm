(load "../lib/util.scm")

; Solution
(define (averager a b c)
  (define e (make-connector))
  (define half (make-connector))

  (adder a b e)
  (multiplier c twice e)
  (constant 2 twice))
