; Solution
(define (for-each f lst)
  (cond ((null? lst) #t)
        (else (f (car lst))
              (for-each f (cdr lst)))))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(for-each (lambda (x) (newline) (display x)) (list 57 321 88))
