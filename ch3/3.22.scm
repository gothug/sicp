(load "../lib/util.scm")

; Solution
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))

    (define (empty-queue?)
      (null? front-ptr))

    (define (insert-queue! el)
      (let ((new-pair (cons el '())))
        (if (empty-queue?)
            (begin
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair))
            (begin
              (set-cdr! rear-ptr new-pair)
              (set! rear-ptr new-pair))))
      dispatch)

    (define (delete-queue!)
      (if (null? front-ptr)
          (error "Delete on empty queue!")
          (set! front-ptr (cdr front-ptr)))
      dispatch)

    (define (print-queue)
      (display front-ptr)
      (new-pair))

    (define (get-queue-list)
      front-ptr)

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'get-queue-list) (get-queue-list))
            ((eq? m 'print-queue) (print-queue))
            ))
    dispatch))

; Tests
(run-tests-header)

(test "make-queue")
(define q (make-queue))
(should-be (q 'empty-queue?) #t)

(test "insert-queue")
(define q-returned ((q 'insert-queue!) 'a))
(should-be (q-returned 'get-queue-list) '(a))

((q 'insert-queue!) 'b)
(should-be (q 'get-queue-list) '(a b))

((q 'insert-queue!) 'c)
(should-be (q 'get-queue-list) '(a b c))

(test "delete-queue")
(q 'delete-queue!)
(should-be (q 'get-queue-list) '(b c))

(q 'delete-queue!)
(should-be (q 'get-queue-list) '(c))

(q 'delete-queue!)
(should-be (q 'get-queue-list) '())
