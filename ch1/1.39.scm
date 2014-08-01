(define (tan-cf x k)

  (define (n i)
    (if (= i 1)
        x
        (* x x -1)))

  (define (d i) (- (* i 2) 1))

  (cont-frac n d k))
