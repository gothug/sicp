(load "../lib/util.scm")
(load "../lib/stream.scm")

; Solution
; a
(define (integrate-series s)
  (define (loop stream coeff)
    (cons-stream
     (/ (stream-car stream) coeff)
     (loop (stream-cdr stream) (+ coeff 1))))
  (loop s 1))

; b
(define (neg-stream s) (stream-map (lambda (x) (- x)) s))
(define cosine-series (cons-stream 1 (neg-stream (integrate-series sine-series))))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))

; Tests
(run-tests-header)

; a
(test "integrage-series")

(define s (cons-stream 1 s))

(define si (integrate-series s))
(should-be (stream-ref si 0) 1)
(should-be (stream-ref si 1) 1/2)
(should-be (stream-ref si 2) 1/3)
(should-be (stream-ref si 3) 1/4)

; b
(test "cosine-series")
(should-be (stream-ref cosine-series 0) 1)
(should-be (stream-ref cosine-series 1) 0)
(should-be (stream-ref cosine-series 2) -1/2)
(should-be (stream-ref cosine-series 3) 0)
(should-be (stream-ref cosine-series 4) 1/24)

(test "sine-series")
(should-be (stream-ref sine-series 0) 0)
(should-be (stream-ref sine-series 1) 1)
(should-be (stream-ref sine-series 2) 0)
(should-be (stream-ref sine-series 3) -1/6)
(should-be (stream-ref sine-series 4) 0)
