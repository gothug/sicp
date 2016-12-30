(load "../lib/interpreter.scm")
(load "../lib/util.scm")
(load "../lib/table.scm")

; Solution
(define (eval-and exp env)
  (if (null? exp)
      #t
      (let ((exp-val (eval (car exp) env)))
        (if exp-val
            (if (null? (cdr exp))
                exp-val
                (eval-and (cdr exp) env))
            #f))))

(define (eval-or exp env)
  (if (null? exp)
      #f
      (let ((exp-val (eval (car exp) env)))
        (if exp-val
            exp-val
            (eval-or (cdr exp) env)))))

(put 'and eval-and)
(put 'or eval-or)

; Tests
(run-tests-header)

(define env '())

(test "and")
  (should-be (eval '(and) env) #t)
  (should-be (eval '(and 1) env) 1)
  (should-be (eval '(and 1 2) env) 2)
  (should-be (eval '(and 1 #f 2) env) #f)

(test "or")
  (should-be (eval '(or) env) #f)
  (should-be (eval '(or 1) env) 1)
  (should-be (eval '(or 1 2) env) 1)
  (should-be (eval '(or 1 2 #f) env) 1)
  (should-be (eval '(or #f #f) env) #f)
