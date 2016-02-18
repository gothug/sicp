(load "../lib/util.scm")

; Solution
(define (rand-update x)
  (+ x 100))

(define new-rand
  (let ((x 0))
    (lambda (action)
      (cond ((eq? action 'generate)
             (begin (set! x (rand-update x))
                  x))
          ((eq? action 'reset)
           (lambda (initial-val)
             (set! x initial-val)))))))

; Tests
(run-tests-header)

(test "rand-update 'generate")
(should-be (new-rand 'generate) 100)
(should-be (new-rand 'generate) 200)
(should-be (new-rand 'generate) 300)

(test "rand-update 'reset")
((new-rand 'reset) 10)
(should-be (new-rand 'generate) 110)
(should-be (new-rand 'generate) 210)
(should-be (new-rand 'generate) 310)
