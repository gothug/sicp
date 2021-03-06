(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (divides? a b) (= (remainder b a) 0))
  (define (next n)
    (if (= n 2) 3 (+ n 2)))

  (find-divisor n 2))
