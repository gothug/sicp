(load "../lib/util.scm")
(load "../lib/funprog.scm")

(define (make-frame-1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; Solution
(define origin-frame-1 car)
(define edge1-frame-1 (compose car cdr))
(define edge2-frame-1 (compose car (compose cdr cdr)))

(define origin-frame-2 car)
(define edge1-frame-1 (compose car cdr))
(define edge2-frame-1 (compose cdr cdr))
