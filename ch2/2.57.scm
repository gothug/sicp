(load "../lib/util.scm")

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-exponentiation base n)
  (cond ((= n 0) 1)
        ((= n 1) base)
        (else (list '** base n))))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (b (base exp)))
           (make-product
            n
            (make-product
             (make-exponentiation b (- n 1))
             (deriv b var)))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))

; Solution
(define (rest-terms exp)
  (let ((op (car exp))
        (2nd-term (caddr exp))
        (rest-terms (cdddr exp)))
    (if (null? rest-terms)
        2nd-term
        (cons op (cons 2nd-term rest-terms)))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((sum? a2) (cons '+ (cons a1 (cdr a2))))
        (else (list '+ a1 a2))))
(define augend rest-terms)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((product? m2) (cons '* (cons m1 (cdr m2))))
        (else (list '* m1 m2))))
(define multiplicand rest-terms)

; Tests
(run-tests-header)

(test "augend")
(define s1 '(+ x 5))
(should-be (addend s1) 'x)
(should-be (augend s1) 5)

(define s2 '(+ x x x))
(should-be (addend s2) 'x)
(should-be (augend s2) '(+ x x))

(test "deriv sum")
(should-be (deriv '(+ x 5) 'x) 1)
(should-be (deriv '(+ x x x) 'x) 3)
(should-be (deriv '(+ (** x 3) (** x 2) x) 'x) '(+ (* 3 (** x 2)) (* 2 x) 1))

(test "deriv product")
(should-be (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
