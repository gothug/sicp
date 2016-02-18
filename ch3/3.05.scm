(load "../lib/util.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

; Solution
(define (get-P x-center y-center r)
  (lambda (x y)
    (<= (+ (square (- x x-center)) (square (- y y-center))) (square r))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2) (random-in-range y1 y2)))

  (monte-carlo trials experiment))

; Tests
(run-tests-header)

(test "estimate-integral")

(define (estimate-pi trials)
  (define P (get-P 1 1 1))
  (define square-area (square 2))
  (define proportion (estimate-integral P 0.0 2.0 0.0 2.0 trials))
  (define circle-area (* proportion square-area))

  (display "estimated pi: ")
  (display (+ circle-area 0.0))
  (newline))

(define trials 1000000)
(estimate-pi trials)
(estimate-pi trials)
(estimate-pi trials)
(estimate-pi trials)
(estimate-pi trials)
(estimate-pi trials)
(estimate-pi trials)
(estimate-pi trials)
