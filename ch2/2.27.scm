; Solution
(define (deep-reverse x)
  (cond ((null? x) (list))
        ((pair? x)
         (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))
        (else x)))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)

(define x (list (list 1 2) (list 3 4)))
(display "((4 3) (2 1)) = ")(display (deep-reverse x))(newline)

(define y (list (list 1 2) (list 3 (list 4 5 6))))
(display "(((6 5 4) 3) (2 1)) = ")(display (deep-reverse y))(newline)
