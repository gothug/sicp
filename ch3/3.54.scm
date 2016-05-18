(load "../lib/util.scm")
(load "../lib/stream.scm")

; Solution
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream
   1
   (mul-streams
    (stream-cdr integers)
    factorials)))

; Tests
(run-tests-header)

(test "factorials")
(should-be (stream-ref factorials 0) 1)
(should-be (stream-ref factorials 1) 2)
(should-be (stream-ref factorials 2) 6)
(should-be (stream-ref factorials 3) 24)
(should-be (stream-ref factorials 4) 120)
(should-be (stream-ref factorials 5) 720)
