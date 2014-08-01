(define (cube-root x) (cube-root-iter 1.0 x))

(define (cube-root-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-root-iter (improve guess x) x)))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define
  (improve guess x)
  (/ (+ guess (/ (+ (/ x (square guess)) (* 2 guess)) 3)) 2)
)

(define (good-enough? guess x) (< (abs (- (cube guess) x)) (* guess 0.0001)))
