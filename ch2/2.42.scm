(load "../lib/util.scm")
(load "../lib/math.scm")
(load "../lib/funprog.scm")

; Solution
(define empty-board (list))

(define (adjoin-position row col rest-of-queens)
  (append rest-of-queens (list row)))

(define (is-safe? c1 r1 c2 r2)
  (cond ((= c1 c2) #f)
        ((= r1 r2) #f)
        ((= (abs (- r1 r2)) (abs (- c1 c2))) #f)
        (else #t)))

(define (safe? col positions)
  (define row (list-ref positions (- col 1)))
  (define (loop c)
    (define r (list-ref positions (- c 1)))
    (cond ((= c col) #t)
          ((is-safe? c r col row) (loop (+ c 1)))
          (else #f)))
  (loop 1))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; Tests
(run-tests-header)

(test "adjoin-position")
(should-be (adjoin-position 3 0 (list 1 2)) (list 1 2 3))

(test "is-safe?")
(should-be (is-safe? 1 2 1 3) #f)
(should-be (is-safe? 1 2 8 2) #f)
(should-be (is-safe? 7 1 8 2) #f)
(should-be (is-safe? 4 6 8 2) #f)
(should-be (is-safe? 2 8 7 1) #t)

(test "safe?")
(should-be (safe? 1 '(9)) #t)
(should-be (safe? 3 '(1 3 2)) #f)

(test "queens")
(should-be (queens 1) (list '(1)))
(should-be (queens 2) (list))
(should-be (queens 3) (list))
(should-be (queens 4) (list '(2 4 1 3) '(3 1 4 2)))
(should-be (length (queens 5)) 10)
(should-be (length (queens 6)) 4)
(should-be (length (queens 7)) 40)
(should-be (length (queens 8)) 92)
