(load "../lib/interpreter.scm")
(load "../lib/interpreter-analyzed.scm")
(load "../lib/util.scm")

; Solution
; code in lib/interpreter.scm

; Tests
(run-tests-header)

(define the-global-environment (setup-environment))

(eval '(+ 1 2) the-global-environment)

;(test "lookup-variable-value")
;  (should-be (eval '(begin (define x 3) x) the-global-environment)
;             3)
