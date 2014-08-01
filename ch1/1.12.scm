(define (pascal r c)
  (cond ((or (< c 1) (> c r) (< r 1)) 0)
        ((or (= c 1) (= c r)) 1)
        (else (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))))
