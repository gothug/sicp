; Solution
(define (even? n) (= (remainder n 2) 0))
(define (odd? n)  (= (remainder n 2) 1))

(define (get-parity n) (if (even? n) even? odd?))

(define (filter lst predicate?)
  (define (filter-iter source-lst result-lst)
    (if (null? source-lst)
        result-lst
        (filter-iter
         (cdr source-lst)
         (append
          result-lst
          (let ((head (car source-lst)))
               (if (predicate? head)
                   (list head)
                   (list)))))))

  (filter-iter lst (list)))

(define (same-parity . l)
  (if (null? l)
      (list)
      (let ((head (car l))
            (tail (cdr l)))
           (cons head (filter tail (get-parity head))))))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)
(display "(1 3 5 7) = ")(display (same-parity 1 2 3 4 5 6 7))(newline)
(display "(2 4 6)   = ")(display (same-parity 2 3 4 5 6 7))  (newline)
(display "(2)       = ")(display (same-parity 2 3))          (newline)
(display "(2 4)     = ")(display (same-parity 2 4))          (newline)
(display "(2)       = ")(display (same-parity 2))            (newline)
(display "(1)       = ")(display (same-parity 1))            (newline)
(display "()        = ")(display (same-parity))              (newline)
