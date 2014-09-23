(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Solution
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)

(display "Result:    ")(display (horner-eval 2 (list 1 3 0 5 0 1)))(newline)
(display "Should be: ")(display "79")(newline)
