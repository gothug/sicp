; #### a.
(define (cont-frac n d k)

  (define (helper i)
    (if (> i k)
        0
        (/ (n i) (+ (d i) (helper (+ i 1))))))

  (helper 1))

; > (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
; Value: .6180555555555556, this is an answer accurate up to 4 decimal places

; #### b.
(define (cont-frac n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))

  (iter k 0))

; > (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
; Value: .6180555555555556, this is an answer accurate up to 4 decimal places
