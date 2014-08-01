; works for n >= 2
(define (exhaustive-fermat-test? n)
    (exhaustive-fermat-test-iter? n 1))

(define (exhaustive-fermat-test-iter? n a)
  (define (try-it?)
    (= (expmod a n n) a))
  (cond ((= a n) true)
        ((try-it?) (exhaustive-fermat-test-iter? n (+ a 1)))
        (else false)))

; using Fermat test
(exhaustive-fermat-test? 561)   ; -> #t
(exhaustive-fermat-test? 1105)  ; -> #t
(exhaustive-fermat-test? 1729)  ; -> #t
(exhaustive-fermat-test? 2465)  ; -> #t
(exhaustive-fermat-test? 2821)  ; -> #t
(exhaustive-fermat-test? 6601)  ; -> #t
