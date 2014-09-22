(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Solution
(define (map p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   (list)
   sequence))

(define (append seq1 seq2)
  (accumulate
   cons
   seq2
   seq1))

(define (length sequence)
  (accumulate
   (lambda (x y) (+ 1 y))
   0
   sequence))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(define x (list 1 2 3))

(display "x:            ")(display x)             (newline)
(display "map square x: ")(display (map square x))(newline)

(define a (list 1 2 3))
(define b (list 4 5 6))
(display "a:          ")(display a)           (newline)
(display "b:          ")(display b)           (newline)
(display "append a b: ")(display (append a b))(newline)

(define c (list 1 2 3 4 5 6 7))
(display "c:        ")(display c)         (newline)
(display "length c: ")(display (length c))(newline)
