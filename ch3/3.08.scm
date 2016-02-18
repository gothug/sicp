; Solution
(define gen-f
  (lambda ()
    (let ((a 1))
      (lambda (x)
        (set! a (* a x))
        a))))

; Tests
(run-tests-header)

(test "left to right")
(define f1 (gen-f))
(define left-first (f1 0))
(define right-second (f1 1))
(should-be (+ left-first right-second) 0)

(test "right to left")
(define f2 (gen-f))
(define right-first (f2 1))
(define left-second (f2 0))
(should-be (+ right-first left-second) 1)
