(load "../lib/interpreter.scm")
(load "../lib/util.scm")
(load "../lib/table.scm")

; Solution
(define (apply procedure arguments) (cons "applying" (cons procedure arguments)))          ; dummy apply

; Tests
(run-tests-header)

(define env '())

(test "ordinary lambda")
  (should-be (eval '(lambda (x) ("op1" x) ("op2" x)) env) '(procedure (x) (("op1" x) ("op2" x)) ()))

(test "let->combination")
  (should-be (let->combination '(let ((a 1) (b 2)) (list a b)))
             '((lambda (a b) (list a b)) 1 2))
