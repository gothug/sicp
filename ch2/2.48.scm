(load "../lib/util.scm")
(load "../lib/funprog.scm")

(define (make-segment spv epv)
  (cons spv epv))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))
