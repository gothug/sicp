(define (log2 n) (/ (log n) (log 2)))

(define (average-damp-count n)
  (floor (log2 n)))

(define (n-root x n)
  (fixed-point ((repeated average-damp (average-damp-count n)) (lambda (y) (/ x (expt y (- n 1))))) 1.0))
