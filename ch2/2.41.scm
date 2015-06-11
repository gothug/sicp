(load "../lib/util.scm")
(load "../lib/math.scm")
(load "../lib/funprog.scm")

; Solution
(define (contains lst e)
  (if (null? lst)
      #f
      (if (= (car lst) e)
          #t
          (contains (cdr lst) e))))

(define (unique-lists m n)
  (if (= m 0)
      (list (list))
      (flatmap
       (lambda (lst)
         (map
          (lambda (i)
            (cons i lst))
          (filter
           (lambda (e)
            (not (contains lst e)))
           (enumerate-interval 1 n))))
       (unique-lists (- m 1) n))))

(define (unique-triples-with-sum n s)
  (filter
    (lambda (triple)
      (= (accumulate + 0 triple) s))
    (unique-lists 3 n)))

; Tests
(run-tests-header)

(test "unique-lists")
(should-be (unique-lists 0 0) (list (list)))
(should-be (unique-lists 0 9) (list (list)))
(should-be (unique-lists 1 3) (list (list 1) (list 2) (list 3)))
(should-be (unique-lists 2 3)
           (list (list 2 1) (list 3 1) (list 1 2) (list 3 2) (list 1 3) (list 2 3)))
(should-be (unique-lists 3 3)
           (list
            (list 3 2 1)
            (list 2 3 1)
            (list 3 1 2)
            (list 1 3 2)
            (list 2 1 3)
            (list 1 2 3)))

(test "unique-triples-with-sum")
(should-be (unique-triples-with-sum 3 6)
           (list
            (list 3 2 1)
            (list 2 3 1)
            (list 3 1 2)
            (list 1 3 2)
            (list 2 1 3)
            (list 1 2 3)))
(should-be (unique-triples-with-sum 3 5) (list))

