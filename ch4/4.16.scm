(load "../lib/interpreter.scm")
(load "../lib/util.scm")
(load "../lib/table.scm")

; Solution
; code in lib/interpreter.scm

; Tests
(run-tests-header)

(define the-global-environment (setup-environment))

(test "lookup-variable-value")
  (should-be (eval '(begin (define x 3) x) the-global-environment)
             3)

(test "scan-out-defines")
  (should-be
   (scan-out-defines
    '((define x (+ 5 1))
      (define y (* 5 2))
      (* x y)))
   '((let ((x '*unassigned*) (y '*unassigned*)) (set! x (+ 5 1)) (set! y (* 5 2)) (* x y))))

  (should-be
   (scan-out-defines '(* 2 3))
   '(* 2 3))

(test "lambda")
  (should-be
   (eval
    '((lambda (y) (define x (+ 5 1)) (* x y)) 2)
    the-global-environment)
   12)
