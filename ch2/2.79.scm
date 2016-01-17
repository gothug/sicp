(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

; Solution
(define (equ? x y) (apply-generic 'equ? x y))

; number pkg
(put 'equ? '(scheme-number scheme-number) =)

; rational pkg
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(put 'equ? '(rational rational) equal-rat?)

; complex pkg
(put 'equ? '(complex complex)
     (lambda (x y) (and
                    (= (real-part x) (real-part y))
                    (= (imag-part x) (imag-part y)))))
