(load "../lib/interpreter.scm")
(load "../lib/util.scm")

(define (define-variable! var val env) (set! global-var val)) ; dummy define
(define (apply-meta procedure arguments) (car arguments))     ; dummy apply
(define (self-evaluating? exp) ; to stop evaluation upon a symbol too, so far
  (cond ((number? exp) true)
        ((string? exp) true)
        ((symbol? exp) true)
        (else false)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((application? exp)
         (apply-meta (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((definition? exp) (eval-definition exp env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

; Solution
(define (application? exp) (tagged-list? exp 'call))

; Tests
(run-tests-header)

(define global-var "")
(define env '())

(test "definition")
  (eval '(define x 4) env)
  (should-be global-var 4)

(test "apply-meta")
  (should-be (eval '(call 1 2 3 4 5) env) 1)
