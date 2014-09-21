; Solution
(define (tree-map f tree)
  (map
   (lambda (x)
     (if (pair? x)
         (tree-map f x)
         (f x)))
   tree))

(define (square-tree tree)
  (tree-map square tree))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(display x)(newline)
(display "(1 (4 (9 16) 25) (36 49)) = ")(display (square-tree x))(newline)
