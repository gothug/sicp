; Solution
(define (square-tree tree)
  (cond ((null? tree) (list))
        ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree-2 tree)
  (map
   (lambda (x)
     (if (pair? x)
         (square-tree-2 x)
         (square x)))
   tree))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(display x)(newline)
(display "(1 (4 (9 16) 25) (36 49)) = ")(display (square-tree x))(newline)
(display "(1 (4 (9 16) 25) (36 49)) = ")(display (square-tree-2 x))(newline)
