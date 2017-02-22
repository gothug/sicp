(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        ((amb? exp) (analyze-amb exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

;(define (analyze-assignment exp)
;  (let ((var (assignment-variable exp))
;        (vproc (analyze (assignment-value exp))))
;    (lambda (env)
;      (set-variable-value! var (vproc env) env)
;      'ok)))

;(define (analyze-if exp)
;  (let ((pproc (analyze (if-predicate exp)))
;        (cproc (analyze (if-consequent exp)))
;        (aproc (analyze (if-alternative exp))))
;    (lambda (env)
;      (if (true? (pproc env))
;          (cproc env)
;          (aproc env)))))


;(define (analyze-application exp)
;  (let ((fproc (analyze (operator exp)))
;        (aprocs (map analyze (operands exp))))
;    (lambda (env)
;      (execute-application (fproc env)
;                           (map (lambda (aproc) (aproc env))
;                                aprocs)))))

;(define (execute-application proc args)
;  (cond ((primitive-procedure? proc)
;         (apply-primitive-procedure proc args))
;        ((compound-procedure? proc)
;         ((procedure-body proc)
;          (extend-environment (procedure-parameters proc)
;                              args
;                              (procedure-environment proc))))
;        (else
;         (error
;          "Unknown procedure type -- EXECUTE-APPLICATION"
;          proc))))
