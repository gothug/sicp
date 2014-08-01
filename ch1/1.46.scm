(define (iterative-improve good-enough improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough guess next)
        next
        (iter next))))
  iter)

(define (sqrt x)
  (define (good-enough guess next)
    (< (abs (- (square guess) x)) 0.001))

  (define (improve guess)
    (average guess (/ x guess)))

  ((iterative-improve good-enough improve) 1.0))

(define (fixed-point f first-guess)
  (define tolerance 0.00001)

  (define (good-enough guess next)
    (< (abs (- guess next)) tolerance))

  (define improve f)

  ((iterative-improve good-enough improve) first-guess))
