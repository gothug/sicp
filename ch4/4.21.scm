(load "../lib/util.scm")
(load "../lib/interpreter.scm")

; Solution

; a
(define fib
  (lambda (n)
   ((lambda (fib) (fib fib n))
    (lambda (ft k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (ft ft (- k 2)) (ft ft (- k 1)))))))))

; b
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

; Tests
(run-tests-header)

(define the-global-environment (setup-environment))

(test "fib")
  (should-be (fib 10) 55)
