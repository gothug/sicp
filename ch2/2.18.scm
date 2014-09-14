; Solution
(define (reverse l)
  (define (reverse-iter lrem lres)
    (if (null? lrem)
        lres
        (reverse-iter (cdr lrem) (cons (car lrem) lres))))
  (reverse-iter l (list)))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(display "(3 2 1) = ")(display (reverse (list 1 2 3)))(newline)
(display "(1)     = ")(display (reverse (list 1)))(newline)
(display "()      = ")(display (reverse '()))(newline)
