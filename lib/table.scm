(load "../lib/util.scm")

(define table '())

(define (put key value)
  (set! table (cons (list key value) table)))

(define (get key)
  (define (search key t)
    (cond
     ((null? t) #f)
     ((eqv? (caar t) key) (cadar t))
     (else (search key (cdr t)))))
  (search key table))

; Tests
;(run-tests-header)
;(put 'key1 1)

;(should-be (get 'key1) 1)
;(should-be (get 'key2) #f)
