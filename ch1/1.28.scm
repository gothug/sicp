(define (square x) (* x x))

(define (miller-rabin-test n)
  (define (check-expmod-result r)
    (cond ((= r 0) false)
          ((= r 1) true)
          (else false)))

    (define (try-it a)
      (check-expmod-result (expmod a (- n 1) n)))

  (try-it (+ 1 (random (- n 1)))))


(define (expmod base exp m)
  (define (sq-check x)
    (if (and (not (or (= x 1) (= x (- m 1)))) (= (remainder (square x) m) 1)) 0
        (remainder (square x) m)))

  (cond ((= exp 0)   1)
        ((even? exp) (sq-check (expmod base (/ exp 2) m)))
        (else        (remainder (* base (expmod base (- exp 1) m)) m))))

; works for n >= 2
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(fast-prime? 561 10)     ; -> #f
(fast-prime? 1105 10)    ; -> #f
(fast-prime? 1729 10)    ; -> #f
(fast-prime? 2465 10)    ; -> #f
(fast-prime? 2821 10)    ; -> #f
(fast-prime? 6601 10)    ; -> #f

; (define (t n times)
;     (cond ((= times 0) false)
;           ((fast-prime? n 5) true)
;           (else (t n (- times 1)))))
