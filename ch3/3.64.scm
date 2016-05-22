(load "../lib/util.scm")
(load "../lib/stream.scm")

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

; Solution
(define (stream-limit s tolerance)
  (let ((h (stream-car s))
        (t (stream-cdr s)))
    (if (< (abs (- h (stream-car t))) tolerance)
        (stream-car t)
        (stream-limit t tolerance))))

; Tests
(run-tests-header)

(test "stream-limit")

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(newline)
(display "Square root:")
(newline)
(display (sqrt 2 0.1))
(newline)
