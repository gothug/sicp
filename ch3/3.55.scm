(load "../lib/util.scm")
(load "../lib/stream.scm")

; Solution
(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (partial-sums s) (stream-cdr s))))

; Tests
(run-tests-header)

(test "partial-sums")
(define s (partial-sums integers))
(should-be (stream-ref s 0) 1)
(should-be (stream-ref s 1) 3)
(should-be (stream-ref s 2) 6)
(should-be (stream-ref s 3) 10)
(should-be (stream-ref s 4) 15)
(should-be (stream-ref s 5) 21)
