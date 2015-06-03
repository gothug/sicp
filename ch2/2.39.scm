; -- utils --
(define (run-tests-header)
  (newline)
  (display "Running tests:")(newline)
  (display "==============")(newline))

(define (should-be a b)
  (display a)(display " should be..")(newline)
  (display b)(newline)

  (if (equal? a b)
      (display "- Passed")
      (display "- Failed!"))
  (newline)(newline))
; -- utils --

; Solution
(define nil (list))

(define (reverse sequence)
  (fold-right (lambda (x y) <??>) nil sequence))

; Tests
(run-tests-header)
