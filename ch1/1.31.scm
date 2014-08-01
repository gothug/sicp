; a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; factorial, works for x >= 0
(define (factorial x)
  (define (id x) x)
  (define (inc x) (+ x 1))

  (product id 1 inc x))

; calculate pi, n >= 1
(define (pi-wallis n)
  (define (inc x) (+ x 1))

  (define (term x)
    (define dblx (* 2 x))
    (* (/ dblx (- dblx 1))
       (/ dblx (+ dblx 1))))

  (* 2.0 (product term 1 inc n)))

(pi-wallis 10)
3.067703806643499
(pi-wallis 100)
3.1337874906281624
(pi-wallis 1000)
3.1408077460303945
(pi-wallis 10000)
3.141514118681922

; b)
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))

  (iter a 1))

(pi-wallis 100000)
3.1415847996572466
