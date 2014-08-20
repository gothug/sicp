(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; Solution
(define (make-rat n d)
  (if (< d 0)
      (make-rat (* n -1) (* d -1))
      (let ((g (gcd n d)))
        (cons-helper (/ n g) (/ d g)))))

(define (cons-helper n d)
  (if (< d 0)
      (cons-helper (* n -1) (* d -1))
      (cons n d)))

; Tests
(print-rat (make-rat 1 3))   ; [ => 1/3 ]
(print-rat (make-rat -1 3))  ; [ => -1/3  ]
(print-rat (make-rat 1 -3))  ; [ => -1/3  ]
(print-rat (make-rat -1 -3)) ; [ => 1/3   ]
