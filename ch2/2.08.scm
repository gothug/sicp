(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Solution
(define (sub-interval x y)
  (make-interval
   (- (lower-bound x) (upper-bound y))
   (- (upper-bound x) (lower-bound y))))

; Tests
(define a (make-interval 1.5 2.5))
(define b (make-interval 6.5 9.5))

(newline)
(display "(4. . 8.)   -> ")(display (sub-interval b a))
(newline)
(display "(-8. . -4.) -> ")(display (sub-interval a b))
