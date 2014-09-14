(define (make-interval a b) (cons a b))

(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Solution
(define (make-center-percent c tol)
  (let ((width (/ (* c tol) 100)))
    (make-interval (- c width) (+ c width))))

(define (percent x)
  (* (/ (width x) (center x)) 100))

; Tests
(define a (make-center-percent 20 0.5))

(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(display "[19.9, 20.1]   = ")(display a)(newline)
(display "~ 0.5          = ")(display (percent a))(newline)
