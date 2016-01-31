(load "../lib/util.scm")

; Solution
(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else (set! counter (+ counter 1))
                  (f x))))))

; Tests
(run-tests-header)

(test "make-monitored")
(define s (make-monitored sqrt))

(should-be (s 100) 10)
(should-be (s 100) 10)
(should-be (s 'how-many-calls?) 2)

(s 'reset-count)
(should-be (s 'how-many-calls?) 0)
