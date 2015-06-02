; -- utils --
(define (run-tests-header)
  (newline)
  (display "Running tests:")(newline)
  (display "==============")(newline))

(define (should-be a b)
  (display a)(display " should be..")(newline)
  (display b)(newline)

  (if (equal? a b)
      (display "- Passed")
      (display "- Failed!"))
  (newline)(newline))
; -- utils --

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Solution
(define (empty-rowed-matrix? m)
  (null? (car m)))

(define (matrix-*-vector m v)
  (map (lambda (k) (dot-product v k)) m))

(define (transpose n)
  (if (empty-rowed-matrix? n)
      (list)
      (cons (map car n) (transpose (map cdr n)))))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

; Tests
(run-tests-header)

(define m (list (list 1 2) (list 3 4) (list 5 6)))
(define v (list 10 20))
(should-be (matrix-*-vector m v) (list 50 110 170))

(should-be (transpose m) (list (list 1 3 5) (list 2 4 6)))

(define n (list (list 9) (list 9) (list 9)))
(should-be (transpose n) (list (list 9 9 9)))

(define m
  (list
    (list 1 2)
    (list 1 3)
    (list 1 4)))
(define n
  (list
    (list 10 100 1000)
    (list 10 100 1000)))
(define m*n
  (list
    (list 30 300 3000)
    (list 40 400 4000)
    (list 50 500 5000)))
(should-be (matrix-*-matrix m n) m*n)
