(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-iter b counter a)
  (cond ((= counter 0) a)
        ((even? counter) (fast-expt-iter (* b b) (/ counter 2) a))
        (else (fast-expt-iter b (- counter 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))
