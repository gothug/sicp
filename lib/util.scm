; -- Helper functions for tests
(define (run-tests-header)
  (newline)
  (display "Running tests")(newline)
  (display "=============")(newline))

(define (test str)
  (display str)(display ":")(newline))

(define (indent)
  (display "  "))

(define (should-be a b)
  (indent)(display a)(display " should be..")(newline)
  (indent)(display b)(newline)

  (indent)
  (if (equal? a b)
      (display "- Passed")
      (display "- Failed!"))
  (newline)(newline))
; -- End of helper functions for tests
