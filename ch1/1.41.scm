; #### 1.
(define (double f)
  (lambda (x) (f (f x))))

; #### 2.
(((double (double double)) inc) 5)

  (double double) =
    (lambda (x) (double (double x))))

;->
(((double (lambda (x) (double (double x)))) inc) 5)

  (double (lambda (x) (double (double x))))

  (double foo) =
    (lambda (x) (foo (foo x))))

  (lambda (x) (double (double (double (double x)))))

;->
(((lambda (x) (double (double (double (double x))))) inc) 5)

;->
((double (double (double (double inc)))) 5)

;->
((double (double (double (lambda (x) (inc (inc x)))))) 5)

;->
((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)

;->
((double (lambda (x) (inc (inc ( inc (inc (inc (inc (inc (inc x)))))))))) 5)

;->
((lambda (x) (inc (inc ( inc (inc (inc (inc (inc (inc (inc (inc ( inc (inc (inc (inc (inc (inc x))))))))))))))))) 5)

;->
21
