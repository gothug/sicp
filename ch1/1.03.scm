(define (sq x) (* x x))

(define (sumsq a b) (+ (sq a) (sq b)))

(define (larger x y) (if (< x y) y x))

(define (sqoftwolgst a b c)
    (if
      (= a (larger a b))
      (sumsq a (larger b c))
      (sumsq b (larger a c))))
