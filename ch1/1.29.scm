(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson-rule f a b n) ; n > 0 and n is an even number
    (define h (/ (- b a) n))

    (define (even? n)
      (= (remainder n 2) 0))

    (define (coeff x)
      (cond ((or (= x 0) (= x n)) 1)
            ((even? x) 2)
            (else 4)))

    (define (term x)
      (* (coeff x) (f (+ a (* x h)))))

    (define (inc x) (+ x 1))

    (/ (* h (sum term 0 inc n)) 3))

(simpson-rule cube 0 1 100.0)   ;Value: .24999999999999992

(simpson-rule cube 0 1 1000.0)  ;Value: .2500000000000003
