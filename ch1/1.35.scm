; fi^2 = fi + 1
; fi = 1 + 1/fi

(define (fi)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 1.0))

; > (fi)
; Value: 1.6180327868852458
