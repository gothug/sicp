(load "../lib/util.scm")
(load "../lib/math.scm")
(load "../lib/funprog.scm")

; Solution
(define make-vect cons)

(define xcor-vect car)

(define ycor-vect cdr)

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
	     (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
	     (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect v s)
  (make-vect (* (xcor-vect v) s)
	     (* (ycor-vect v) s)))

; Tests
(run-tests-header)

(test "make-vect")
(should-be (make-vect 2 4) (cons 2 4))

(test "xcor-vect")
(should-be (xcor-vect (make-vect 3 8)) 3)

(test "ycor-vect")
(should-be (ycor-vect (make-vect 3 8)) 8)

(define v1 (make-vect 2 4))
(define v2 (make-vect 3 5))

(test "add-vect")
(should-be (add-vect v1 v2) (make-vect 5 9))

(test "sub-vect")
(should-be (sub-vect v1 v2) (make-vect -1 -1))

(test "scale-vect")
(define v (make-vect 2 4))
(should-be (scale-vect v 3) (make-vect 6 12))
