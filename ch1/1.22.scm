(define (even? n)
  (= (remainder n 2) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes from to)
  (if (not (even? from)) (timed-prime-test from))
  (if (> from (- to 1)) (display "finished")
      (search-for-primes (+ from 1) to)))

(search-for-primes 10000000 10000100)               ; 0.01
(search-for-primes 100000000 100000100)             ; 0.03
(search-for-primes 1000000000 1000000100)           ; 0.07
(search-for-primes 10000000000 10000000100)         ; 0.21
(search-for-primes 100000000000 100000000100)       ; 0.65
(search-for-primes 1000000000000 1000000000100)     ; 2.05
(search-for-primes 10000000000000 10000000000100)   ; 6.52
