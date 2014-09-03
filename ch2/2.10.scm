(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
  (make-interval (min p1 p2 p3 p4)
                 (max p1 p2 p3 p4))))

; Solution
(define (spans-zero? x)
  (<= (* (upper-bound x) (lower-bound x)) 0))

(define (div-interval x y)
  (if (spans-zero? y)
      (error "Error: the denominator interval should not span 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y)) (/ 1.0 (lower-bound y))))))

; Tests
(define a (make-interval -1 0))
(define b (make-interval 1 0))
(define c (make-interval -1 1))
(define d (make-interval 1 -1))
(define e (make-interval 1 2))
(define f (make-interval -1 -2))

(define z (make-interval 8 12))

(newline)
(display "Running tests:")
(newline)
(display "==============")
(newline)
;; (display "ERROR -> ")(display (div-interval z a))
;; (display "ERROR -> ")(display (div-interval z b))
;; (display "ERROR -> ")(display (div-interval z c))
;; (display "ERROR -> ")(display (div-interval z d))
(display "OK    -> ")(display (div-interval z e))
(newline)
(display "OK    -> ")(display (div-interval z f))
