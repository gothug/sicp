; Solution
(define (fringe x)
  (cond ((null? x) (list))
        ((pair? x) (append (fringe (car x)) (fringe (cdr x))))
        (else (list x))))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)

(define x (list (list 1 2) (list 3 4)))
(display "(1 2 3 4)         = ")(display (fringe x))         (newline)
(display "(1 2 3 4 1 2 3 4) = ")(display (fringe (list x x)))(newline)
