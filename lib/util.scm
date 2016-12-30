; -- Helper functions for tests
(define (run-tests-header)
  (newline)
  (display "Running tests")(newline)
  (display "=============")(newline))

(define (test str)
  (display str)(display ":")(newline))

(define (displayn str)
  (display str)(newline))

(define (indent)
  (display "  "))

(define (should-be a b)
  (indent)(display "    Result:   ")(display a)(newline)
  (indent)(display "    Expected: ")(display b)(newline)

  (indent)
  (if (equal? a b)
      (display "- Passed")
      (warn "Failed"))
  (newline)(newline))
; -- End of helper functions for tests
