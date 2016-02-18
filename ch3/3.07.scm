(load "../lib/util.scm")

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
              ((eq? m 'is-correct-secret) #t)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

; Solution
(define (make-joint acc pwd pwd-joint)
  (define (dispatch entered-secret m)
    (if (eq? pwd-joint entered-secret)
        (acc pwd m)
        (error "Incorrect joint password")))

  (if (acc pwd 'is-correct-secret)
      dispatch
      (error "Incorrect original account password")))

; Tests
(run-tests-header)

(test "make-account")

(define peter-acc (make-account 100 'secret-peter))
(should-be ((peter-acc 'secret-peter 'withdraw) 40) 60)
(should-be (peter-acc 'secret-peter 'is-correct-secret) #t)

(define paul-acc (make-joint peter-acc 'secret-peter 'secret-paul))
(should-be ((paul-acc 'secret-paul 'withdraw) 10) 50)
(should-be (paul-acc 'secret-paul 'is-correct-secret) #t)
(should-be ((paul-acc 'secret-peter 'withdraw) 10) 50) ; Incorrect password error
