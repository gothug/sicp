(define (even? n)
  (= (remainder n 2) 0))

(define (mult-iter a b r)
  (cond ((= b 0) r)
        ((even? b) (mult-iter (+ a a) (/ b 2) r))
        (else (mult-iter a (- b 1) (+ r a)))))

(define (mult a b)
  (mult-iter a b 0))
