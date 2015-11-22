(load "../lib/util.scm")

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

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
        (else
         (error "unknown expression type -- DERIV" exp))))

; Solution
(define nil (list))

(define (find-idx list symbol)
  (define (iter list idx)
    (if (null? list)
        -1
        (if (eq? (car list) symbol)
            idx
            (iter (cdr list) (+ idx 1)))))
  (iter list 0))

(define (split list symbol)
  (let ((idx (find-idx list symbol)))
        (if (eq? idx -1)
            (cons list nil)
            (cons (sublist list 0 idx) (sublist list (+ idx 1) (length list))))))
(define (unpack-1el-list l)
  (if (= (length l) 1)
      (car l)
      l))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (pair? a1) (pair? a2)) (append a1 '(+) a2))
        ((pair? a1) (append a1 (list '+ a2)))
        ((pair? a2) (append (list a1 '+) a2))
        (else (list a1 '+ a2))))
(define (sum? x)
  (and
   (pair? x)
   (> (find-idx x '+) -1)))
(define (addend s) (unpack-1el-list (car (split s '+))))
(define (augend s) (unpack-1el-list (cdr (split s '+))))

(define (wrap-to-list exp)
  (cond ((or (sum? exp) (not (pair? exp))) (list exp))
        (else exp)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (append (wrap-to-list m1) '(*) (wrap-to-list m2)))))
(define (product? x)
  (and
   (pair? x)
   (eq? (find-idx x '+) -1)
   (> (find-idx x '*) -1)))
(define (multiplier s) (unpack-1el-list (car (split s '*))))
(define (multiplicand s) (unpack-1el-list (cdr (split s '*))))

; Tests
(run-tests-header)

(test "find-idx")
(define l '())
(should-be (find-idx l '+) -1)

(test "split")
(define l2 '(1 * 2 + 3))
(define sp (split l2 '+))
(should-be (car sp) '(1 * 2))
(should-be (cdr sp) '(3))

(test "sum?")
(should-be (sum? '(1 * 2 + 3)) #t)
(should-be (sum? '(1 * (2 + 3))) #f)

(test "make-sum")
(should-be (make-sum '(a * b) '(c + d + e)) '(a * b + c + d + e))
(should-be (make-sum 'a '(c * d)) '(a + c * d))
(should-be (make-sum 'a 2) '(a + 2))

(test "addend/augend")
(should-be
 (addend (make-sum '(a * b) '(c + d + e)))
 '(a * b))
(should-be
 (augend (make-sum '(a * b) '(c + d + e)))
 '(c + d + e))
(define s1 (make-sum 'a 'b))
(should-be (addend s1) 'a)
(should-be (augend s1) 'b)

(test "product?")
(should-be (product? 'x) #f)
(should-be (product? '(x * y)) #t)
(should-be (product? '(x * y + z)) #f)
(should-be (product? '(x * (y + z))) #t)

(test "make-product")
(should-be (make-product '(a * b) '(c + d)) '(a * b * (c + d)))
(should-be (make-product '(a * b) '(c * d)) '(a * b * c * d))
(should-be (make-product 'a '(b * c)) '(a * b * c))
(should-be (make-product 'a '(b + c)) '(a * (b + c)))
(should-be (make-product '(a * b) 'c) '(a * b * c))
(should-be (make-product '(a + b) 'c) '((a + b) * c))
(should-be (make-product 3 'x) '(3 * x))

(test "multiplier/multiplicand")
(define p1 (make-product '(a + b) '(c * d)))
(should-be p1 '((a + b) * c * d))
(should-be (multiplier p1) '(a + b))
(should-be (multiplicand p1) '(c * d))

(define p2 (make-product 'a '(b + c)))
(should-be p2 '(a * (b + c)))
(should-be (multiplier p2) 'a)
(should-be (multiplicand p2) '(b + c))

(test "deriv")
(should-be (deriv '(x + 3 * (x + y + 2)) 'x) 4)
(should-be
 (deriv '(2 * x * x + x * (y + x) + 5 * x) 'x)
 '(2 * (x + x) + x + y + x + 5))
