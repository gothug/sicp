(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

; Solution

; area & perimeter
(define (perimeter r)
  (+
    (* (rect-width r) 2)
    (* (rect-height r) 2)))

(define (area r)
  (* (rect-width r) (rect-height r)))

; rectangle implementation #1
(define (make-rectangle bottom-left upper-right)
  (cons bottom-left upper-right))

(define (bottom-left r) (car r))

(define (upper-right r) (cdr r))

(define (rect-width r)
  (- (x-point (upper-right r)) (x-point (bottom-left r))))

(define (rect-height r)
  (- (y-point (upper-right r)) (y-point (bottom-left r))))

; test rectangle implementation #1
(define a (make-point -2 -1))
(define b (make-point 1 6))
(define rec (make-rectangle a b))

(newline)
(display (perimeter rec)) ; =20
(newline)
(display (area rec)) ; =21

; rectangle implementation #2
(define (make-rectangle bottom-left height width)
  (cons bottom-left (cons height width)))

(define (rect-width r) (cdr (cdr r)))

(define (rect-height r) (car (cdr r)))

; test rectangle implementation #2
(define a (make-point -2 -1))
(define rec (make-rectangle a 7 3))

(newline)
(display (perimeter rec)) ; =20
(newline)
(display (area rec)) ; =21
