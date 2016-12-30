; misc
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; self-evaluating
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

; variable
(define (variable? exp) (symbol? exp))

; quoted
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

; definition
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; application
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ;((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ;((if? exp) (eval-if exp env))
        ;((lambda? exp)
        ; (make-procedure (lambda-parameters exp)
        ;                 (lambda-body exp)
        ;                 env))
        ;((begin? exp) 
        ; (eval-sequence (begin-actions exp) env))
        ;((cond? exp) (eval (cond->if exp) env))
        (else
         (let ((eval-proc (get (car exp))))
           (if eval-proc
               (eval-proc (cdr exp) env)
               (if (application? exp)
                   (apply (eval (operator exp) env)
                          (list-of-values (operands exp) env))
                   (error "Unknown expression -- EVAL" exp)))))))

; (define (apply procedure arguments)
;   (cond ((primitive-procedure? procedure)
;          (apply-primitive-procedure procedure arguments))
;         ((compound-procedure? procedure)
;          (eval-sequence
;            (procedure-body procedure)
;            (extend-environment
;              (procedure-parameters procedure)
;              arguments
;              (procedure-environment procedure))))
;         (else
;          (error
;           "Unknown procedure type -- APPLY" procedure))))
