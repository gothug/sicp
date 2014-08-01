; #### 1.
(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

; #### 2.
((repeated smooth n) f)
