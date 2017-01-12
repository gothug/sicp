(load "../lib/interpreter.scm")
(load "../lib/interpreter-analyzed.scm")
(load "../lib/util.scm")

; Tests
(define the-global-environment (setup-environment))

(define recursive #f)

(define result
  (if recursive
      (eval
       '(begin
          (define (fact n)
            (if (= n 1)
                n
                (* n (fact (- n 1)))))
          (fact 5000))
       the-global-environment)
      (eval
       '(begin
          (define (fact n)
            (define (loop n acc)
              (if (= n 1)
                  acc
                  (loop (- n 1) (* acc n))))
            (loop n 1))
          (fact 5000))
       the-global-environment)))

;(displayn result)
