(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; Solution
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)

(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(display "Result:    ")(display (accumulate-n + 0 x))(newline)
(display "Should be: ")(display "(22 26 30)")(newline)
