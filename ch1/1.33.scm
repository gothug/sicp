(define (filtered-accumulate predicate? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate? a)
                  (combiner (term a) result)
                  result))))

  (iter a null-value))

; a)
(define (prime-sq-sum a b)
  (define (inc x) (+ x 1))

  (filtered-accumulate prime? + 0 square a inc b))

; b)
(define (rel-prime-int-product n)
  (define (inc x) (+ x 1))
  (define (id x) x)
  (define (predicate? x) (= (gcd n x) 1))

  (filtered-accumulate predicate? * 1 id 1 inc (- n 1)))

(rel-prime-int-product 10)
