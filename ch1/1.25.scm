(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))

; expmod2 takes much longer time to execute because of having to operate with
; very large intermediate values

(expmod 77766 100000 100000)
(expmod2 77766 100000 100000)
