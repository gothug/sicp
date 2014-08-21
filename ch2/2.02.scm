(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Solution
(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (make-point
    (average (x-point (start-segment s)) (x-point (end-segment s)))
    (average (y-point (start-segment s)) (y-point (end-segment s)))))

(define (average a b)
  (/ (+ a b) 2))

; Tests
(print-point (make-point 2 4)) ; (2,4)

(print-point
  (midpoint-segment (make-segment (make-point 1 1) (make-point 6 2)))) ; (7/2,3/2)
