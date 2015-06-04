(load "../lib/util.scm")
(load "../lib/math.scm")

(define nil (list))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

; Solution
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

; Tests
(run-tests-header)

(test "unique-pairs")
(should-be (unique-pairs 0) (list))
(should-be (unique-pairs 1) (list))
(should-be (unique-pairs 2) (list (list 2 1)))
(should-be (unique-pairs 3) (list (list 2 1) (list 3 1) (list 3 2)))

(test "prime-sum-pairs")
(should-be (prime-sum-pairs 0) (list))
(should-be (prime-sum-pairs 1) (list))
(should-be (prime-sum-pairs 2) (list (list 2 1 3)))
(should-be (prime-sum-pairs 3) (list (list 2 1 3) (list 3 2 5)))
(should-be (prime-sum-pairs 4)
           (list (list 2 1 3) (list 3 2 5) (list 4 1 5) (list 4 3 7)))
