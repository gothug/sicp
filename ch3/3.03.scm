(load "../lib/util.scm")

; Solution
(define (make-account balance secret)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch entered-secret m)
    (if (eq? secret entered-secret)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

; Tests
(run-tests-header)

(test "make-account")

(define acc (make-account 100 'secret-password))

(should-be ((acc 'secret-password 'withdraw) 40) 60)

((acc 'some-other-password 'deposit) 50) ; should be error message 'Incorrect password'
