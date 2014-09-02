(define (make-interval a b) (cons a b))

; Solution
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Tests
(newline)
(display "4 -> ")(display (lower-bound (make-interval 4 9)))
(newline)
(display "9 -> ")(display (upper-bound (make-interval 4 9)))
