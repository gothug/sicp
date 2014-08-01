(define (sqrt x) (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x) (/ (+ guess (/ x guess)) 2))

(define (square x) (* x x))

(define (good-enough? guess x) (< (abs (- (square guess) x)) (* guess 0.0001)))
