(load "../lib/util.scm")
(load "../lib/stream.scm")

(define sum 0)
(displayn sum)

(define (accum x)
  (set! sum (+ x sum))
  sum)
(displayn sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(displayn sum)

(define y (stream-filter even? seq))
(displayn sum)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
(displayn sum)

(stream-ref y 7)
(displayn sum)

(display-stream z)
(newline)(displayn sum)
