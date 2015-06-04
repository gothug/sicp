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

; Solution
(define nil (list))

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

; Tests
(run-tests-header)

(should-be (reverse-1 (list 1 2 3)) (list 3 2 1))
(should-be (reverse-2 (list 1 2 3)) (list 3 2 1))

(should-be (reverse-1 (list 1)) (list 1))
(should-be (reverse-2 (list 1)) (list 1))

(should-be (reverse-1 nil) nil)
(should-be (reverse-2 nil) nil)
