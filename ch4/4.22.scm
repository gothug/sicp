(load "../lib/interpreter.scm")
(load "../lib/interpreter-analyzed.scm")
(load "../lib/util.scm")

; Solution
; code in lib/interpreter-analyzed.scm

; Tests
(run-tests-header)

(define the-global-environment (setup-environment))

(test "let")
  (should-be (eval '(let ((x 1) (y 2)) (+ x y)) the-global-environment) 3)
