(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

; normal order evaluation
(gcd 206 40) ->
  (if (= 40 0) 206
    (gcd 40 (remainder 206 40)))

(gcd 40 6:r1) ->
  (if (= 6:r1 0) 40                       ; remainder apply 1 time
    (gcd 6:r1 (remainder 40 6:r1)))

(gcd 6:r1 4:r2) ->
  (if (= 4:r2 0) 6:r1                     ; remainder apply 2 times
    (gcd 4:r2 (remainder 6:r1 4:r2)))

(gcd 4:r2 2:r4) ->
  (if (= 2:r4 0) 4:r2                     ; remainder apply 4 times
    (gcd 2:r4 (remainder 4:r2 2:r4)))

(gcd 2:r4 0:r7) ->
  (if (= 0:r7 0) 2:r4                     ; remainder apply 7 times + 4 times
    (gcd 0:r7 (remainder 2:r4 0:r7)))
;=> remainder applied 18 times


; applicative order evaluation
(gcd 206 40)
(gcd 40 6) ; remainder applied
(gcd 6 4) ; remainder applied
(gcd 4 2) ; remainder applied
(gcd 2 0) ; remainder applied
2
;=> remainder applied 4 times
