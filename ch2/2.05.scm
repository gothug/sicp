; Solution
(define (exp base n)
  (define (exp-accum base n acc)
    (if (= n 0)
        acc
        (exp-accum base (- n 1) (* acc base))))
  (exp-accum base n 1))

(define (div-times z base)
  (define (iter z result)
    (let ((base-sq (* base base)))
         (cond ((= (remainder z base-sq) 0)
                (iter (/ z base-sq) (+ result 2)))
               ((= (remainder z base) 0)
                (iter (/ z base) (+ result 1)))
               (else result))))
  (iter z 0))

(define (cons x y)
  (* (exp 2 x) (exp 3 y)))

(define (car z)
  (div-times z 2))

(define (cdr z)
  (div-times z 3))

; Tests
(define x (cons 141231 128))

(newline)
(display "14  -> ")(display (car x))

(newline)
(display "128 -> ")(display (cdr x))
