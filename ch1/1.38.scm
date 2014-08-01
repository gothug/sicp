; k >= 1
(define (e k)

  (define (n i) 1)

  (define (d i)
    (if (= (remainder (+ i 1) 3) 0)
        (* 2 (/ (+ i 1) 3))
        1))

  (+ (cont-frac n d k) 2))
