(load "../lib/interpreter.scm")
(load "../lib/util.scm")
(load "../lib/table.scm")

; Solution
(define (apply-meta procedure arguments) (cons "applying" (cons procedure arguments)))          ; dummy apply
; special syntax support for cond is implemented in file lib/interpreter.scm

; Tests
(run-tests-header)

(define env '())

(test "ordinary cond")
  (should-be (eval '(cond (#f 0) (#t 1) (else 2)) env) 1)
  (should-be (eval '(cond (#f 0) (#f 1) (else 2)) env) 2)
  (should-be (eval '(cond (else 0)) env) 0)

(test "special form cond")
  (should-be (eval '(cond (#f 0) (1 => "proc") (else 2)) env) '("applying" "proc" 1))
  (should-be (eval '(cond (#f 0) (#f => "proc") (else 2)) env) 2)
  (should-be (eval '(cond (#t 0) (1 => "proc") (else 2)) env) 0)
