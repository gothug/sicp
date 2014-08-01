(define (square x) (* x x))

(define (cube x) (* x x x))

(define (cubic a b c)
  (lambda (y) (+ (cube y) (* a (square y)) (* b y) c)))
