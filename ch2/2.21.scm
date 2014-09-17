; Solution
(define (square-list-1 items)
  (if (null? items)
      (list)
      (cons (square (car items)) (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map square items))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(display "(1 4 9 16 25) = ")(display (square-list-1 (list 1 2 3 4 5)))(newline)
(display "(1 4 9 16 25) = ")(display (square-list-2 (list 1 2 3 4 5)))(newline)
