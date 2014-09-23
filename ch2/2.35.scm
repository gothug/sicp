(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Solution
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)

(define x (cons (list 1 2) (list 3 4)))

(display "Result:    ")(display (count-leaves x))(newline)
(display "Should be: ")(display "4")(newline)

(display "Result:    ")(display (count-leaves (list x x)))(newline)
(display "Should be: ")(display "8")(newline)

(display "Result:    ")(display (count-leaves (list 1 (list x x) 2)))(newline)
(display "Should be: ")(display "10")(newline)
