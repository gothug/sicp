(load "../lib/util.scm")

; Solution
(define (list-has-cycle x)
  (let ((traversed '()))
    (define (has-cycle list)
      (cond ((null? list) #f)
            ((memq list traversed) #t)
            (else
             (begin
               (set! traversed (cons list traversed))
               (has-cycle (cdr list))))))
    (has-cycle x)))

; Tests
(run-tests-header)

(test "list-has-cycle")

(define l (list 'c 'b 'c))
(should-be (list-has-cycle l) #f)

(define cycled-list (list 'a 'b 'c))
(set-cdr! (cddr cycled-list) cycled-list)
(should-be (list-has-cycle cycled-list) #t)
