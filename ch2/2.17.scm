; Solution
(define (last-pair x)
  (define (lp-iter l tail)
    (if (null? tail)
        l
        (lp-iter tail (cdr tail))))
  (lp-iter x (cdr x)))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(display "(3) = ")(display (last-pair (list 1 2 3)))(newline)
(display "(1) = ")(display (last-pair (list 1)))(newline)
