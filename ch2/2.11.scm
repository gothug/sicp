(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

; Solution
(define (sign-pos a b) (and (>= a 0) (>= b 0)))
(define (sign-neg a b) (and (< a 0) (< b 0)))
(define (sign-mix a b) (and (< a 0) (>= b 0)))

(define (mul-interval x y)
  (let ((lx (lower-bound x))
        (hx (upper-bound x))
        (ly (lower-bound y))
        (hy (upper-bound y)))
       (cond ((sign-pos lx hx)
              (cond ((sign-pos ly hy) (make-interval (* lx ly) (* hx hy)))
                    ((sign-neg ly hy) (make-interval (* hx ly) (* lx hy)))
                    ((sign-mix ly hy) (make-interval (* hx ly) (* hx hy)))))
             ((sign-neg lx hx)
              (cond ((sign-pos ly hy) (make-interval (* lx hy) (* hx ly)))
                    ((sign-neg ly hy) (make-interval (* hx hy) (* lx ly)))
                    ((sign-mix ly hy) (make-interval (* lx hy) (* lx ly)))))
             ((sign-mix lx hx)
              (cond ((sign-pos ly hy) (make-interval (* lx hy) (* hx hy)))
                    ((sign-neg ly hy) (make-interval (* hx ly) (* lx ly)))
                    ((sign-mix ly hy) (make-interval
                                       (min (* lx hy) (* hx ly))
                                       (max (* lx ly) (* hx hy)))))))))

; Tests
(define a (make-interval 1 3))
(define b (make-interval -3 -1))
(define c (make-interval -1 1))

(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(display "[1, 9]   = ")(display (mul-interval a a))(newline)
(display "[-9, -1] = ")(display (mul-interval a b))(newline)
(display "[-3, 3]  = ")(display (mul-interval a c))(newline)

(display "[1, 9]   = ")(display (mul-interval b b))(newline)
(display "[-9, -1] = ")(display (mul-interval b a))(newline)
(display "[-3, 3]  = ")(display (mul-interval b c))(newline)

(display "[-1, 1]  = ")(display (mul-interval c c))(newline)
(display "[-3, 3]  = ")(display (mul-interval c a))(newline)
(display "[-3, 3]  = ")(display (mul-interval c b))(newline)
