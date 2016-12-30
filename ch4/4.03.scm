(load "../lib/interpreter.scm")
(load "../lib/util.scm")
(load "../lib/table.scm")

; Solution
(define (apply procedure arguments) (cdr arguments))          ; dummy apply
(define (define-variable! var val env) (set! global-var val)) ; dummy define

(define (eval-application exp env)
   (apply (eval (operator exp) env)
     (list-of-values (operands exp) env)))

(put 'application eval-application)
(put 'definition eval-definition)

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        (else
         (let ((eval-proc (get (car exp))))
           (if eval-proc
               (eval-proc (cdr exp) env)
               (error "Unknown expression type -- EVAL" (car exp)))))))

; Tests
(run-tests-header)

(define global-var "")
(define env '())

(test "number")
  (should-be (eval 4 env) 4)

(test "apply")
  (should-be (eval '(application "op" 1 2 3 4) env) '(2 3 4))

(test "define")
  (eval '(definition define foo 33) env)
  (should-be global-var 33)
