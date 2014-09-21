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

(display "Initial set: ")(display x)(newline)

(display "Result set:  ")(display (subsets x))(newline)

(display "Should be:   ")(display "(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))")
(newline)
