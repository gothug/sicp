(load "../lib/util.scm")

; Solution
(define (triple a b c) (cons a (cons b c)))
(define (cf triple) (car triple))
(define (cm triple) (cadr triple))
(define (cl triple) (cddr triple))
(define (set-f! triple x) (set-car! triple x))
(define (set-m! triple x) (set-car! (cdr triple) x))
(define (set-l! triple x) (set-cdr! (cdr triple) x))

(define (make-deque) (cons '() '()))

(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))
(define (set-front-ptr! dq item) (set-car! dq item))
(define (set-rear-ptr! dq item) (set-cdr! dq item))

(define (empty-deque? dq) (null? (front-ptr dq)))

(define (front-deque dq)
  (if (null? (front-ptr dq))
      (error "Empty deque!")
      (cm (front-ptr dq))))

(define (rear-deque dq)
  (if (null? (rear-ptr dq))
      (error "Empty deque!")
      (cm (rear-ptr dq))))

(define (members-deque dq)
  (define (iter item members)
    (if (null? item)
        members
        (iter (cf item) (cons (cm item) members))))
  (iter (rear-ptr dq) '()))

(define (front-insert-deque! dq item)
  (let ((new-triple (triple '() item '())))
    (if (empty-deque? dq)
        (begin
          (set-front-ptr! dq new-triple)
          (set-rear-ptr! dq new-triple)
          dq)
        (begin
          (set-l! new-triple (front-ptr dq))
          (set-f! (front-ptr dq) new-triple)
          (set-front-ptr! dq new-triple)
          dq))))

(define (rear-insert-deque! dq item)
  (let ((new-triple (triple '() item '())))
    (if (empty-deque? dq)
        (begin
          (set-front-ptr! dq new-triple)
          (set-rear-ptr! dq new-triple)
          dq)
        (begin
          (set-l! (rear-ptr dq) new-triple)
          (set-f! new-triple (rear-ptr dq))
          (set-rear-ptr! dq new-triple)
          dq))))

(define (front-delete-deque! dq)
  (if (empty-deque? dq)
      (error "Delete from empty deque!")
      (begin
        (set-front-ptr! dq (cl (front-ptr dq)))
        (if (null? (front-ptr dq))
            (set-rear-ptr! dq '())
            (set-f! (front-ptr dq) '()))
        dq)))

(define (rear-delete-deque! dq)
  (if (empty-deque? dq)
      (error "Delete from empty deque!")
      (begin
        (set-rear-ptr! dq (cf (rear-ptr dq)))
        (if (null? (rear-ptr dq))
            (set-front-ptr! dq '())
            (set-l! (rear-ptr dq) '()))
        dq)))

; Tests
(run-tests-header)

(test "make-deque")
(define d (make-deque))
(should-be (empty-deque? d) #t)
(should-be (members-deque d) '())

(test "front-insert-deque!")
(front-insert-deque! d 5)
(front-insert-deque! d 4)
(should-be (members-deque d) '(4 5))

(test "rear-insert-deque!")
(rear-insert-deque! d 6)
(rear-insert-deque! d 7)
(should-be (members-deque d) '(4 5 6 7))

(test "front-deque")
(should-be (front-deque d) 4)

(test "rear-deque")
(should-be (rear-deque d) 7)

(test "front-delete-deque!")
(front-delete-deque! d)
(should-be (members-deque d) '(5 6 7))

(front-delete-deque! d)
(should-be (members-deque d) '(6 7))

(front-delete-deque! d)
(should-be (members-deque d) '(7))

(front-delete-deque! d)
(should-be (members-deque d) '())
(should-be (empty-deque? d) #t)

(test "rear-delete-deque!")
(rear-insert-deque! d 2)
(front-insert-deque! d 1)
(rear-insert-deque! d 3)
(should-be (members-deque d) '(1 2 3))

(rear-delete-deque! d)
(should-be (members-deque d) '(1 2))

(rear-delete-deque! d)
(should-be (members-deque d) '(1))

(rear-delete-deque! d)
(should-be (members-deque d) '())
(should-be (empty-deque? d) #t)
