(load "../lib/util.scm")
(load "../lib/stream.scm")

(define (neg-stream s) (stream-map (lambda (x) (- x)) s))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (add-streams (scale-stream (stream-cdr s1)
                                           (stream-car s2))
                             (scale-stream (stream-cdr s2)
                                           (stream-car s1)))
                (cons-stream 0 (mul-series (stream-cdr s1)
                                           (stream-cdr s2))))))

; Solution
(define (invert-unit-series s)
  (define x
    (cons-stream
     1
     (neg-stream
      (mul-series
       (stream-cdr s)
       x))))
  x)
