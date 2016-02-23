(load "../lib/util.scm")

; Solution
(define (count-pairs x)
  (let ((traversed '()))
    (define (count x)
      (if (not (pair? x))
          0
          (if (memq x traversed)
            0
            (begin
              (set! traversed (cons x traversed))
              (+ (count (car x))
                 (count (cdr x))
                 1)))))
    (count x)))

; Tests
(run-tests-header)

(test "count-pairs")
(define p1 (list 1 2 3))
(should-be (count-pairs p1) 3)

(define p2 (cons 'a 'b))
(define p3 (cons p2 (cons p2 '())))
(should-be (count-pairs p3) 3)

(define p4 (cons 'a 'b))
(define p5 (cons p4 p4))
(define p6 (cons p5 p5))
(should-be (count-pairs p6) 3)

(define p7 (list 'a 'b 'c))
(set-cdr! (cddr p7) p7)
(should-be (count-pairs p7) 3)
