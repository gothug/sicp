; Solution
(define (subsets s)
  (if (null? s)
      (list (list))
      (let ((rest (subsets (cdr s))))
        (append
         rest
         (map
          (lambda (x) (cons (car s) x))
          rest)))))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(define x (list 1 2 3))

(display x)(newline)
(display "(1 (4 (9 16) 25) (36 49)) = ")(display (subsets x))(newline)
