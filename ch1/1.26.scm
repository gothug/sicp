; (expmod base (/ exp 2) will be calculated twice because of applicative order
; used by default
;
; so, the multiplications in even? branch grow as 2^n, so O(log n) order growth
; is turned into O(log 2^n) = O(n)
