(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))

; Solution

;      Mobile               Branch(B)
;        |                     |
;    .-------.         .---------------.
;    |       |         |               |
;   L(B)    R(B)      Len    Struct(Number/Mobile)
;

; a.
(define (left-branch x) (car x))
(define (right-branch x) (cadr x))

(define (branch-length x) (car x))
(define (branch-structure x) (cadr x))

; b.
(define (branch-weight y)
  (let ((bs (branch-structure y)))
    (if (pair? bs)
        (total-weight bs)
        bs)))

(define (total-weight x)
  (+ (branch-weight (left-branch x))
     (branch-weight (right-branch x))))

; c.
(define (branch-torque x)
  (* (branch-length x) (branch-weight x)))

(define (balanced? x)
  (let ((l (left-branch x))
        (r (right-branch x)))

    (and (= (branch-torque l) (branch-torque r))
         (branch-balanced? l)
         (branch-balanced? r))))

(define (branch-balanced? x)
  (let ((s (branch-structure x)))
    (if (pair? s)
        (balanced? s)
        #t)))

; d.
(define (left-branch-new x) (car x))
(define (right-branch-new x) (cdr x))

(define (branch-length-new x) (car x))
(define (branch-structure-new x) (cdr x))

; Tests
(newline)
(display "Running tests:")(newline)
(display "==============")(newline)

(define l (make-branch 1 4))
(define r (make-branch 1 8))
(define m (make-mobile l r))

(define x (make-mobile (make-branch 1 m) r))

; a.
(display "(1 ((1 4) (1 8))) = ")(display (left-branch x))(newline)
(display "(1 8)             = ")(display (right-branch x))(newline)

; b.
(display "20                = ")(display (total-weight x))(newline)

; c.
(define l (make-branch 2 4))
(define r (make-branch 1 8))
(define m (make-mobile l r))
(display "#t                = ")(display (balanced? m))(newline)

(define a (make-mobile (make-branch 1 m) (make-branch 1 m)))
(display "#t                = ")(display (balanced? a))(newline)

(define l (make-branch 1 4))
(define r (make-branch 1 8))
(define m (make-mobile l r))
(define b (make-mobile (make-branch 1 m) (make-branch 1 m)))
(display "#f                = ")(display (balanced? b))(newline)

(define l (make-branch 1 4))
(define r (make-branch 1 8))
(define m1 (make-mobile l l))
(define m2 (make-mobile r r))
(define c (make-mobile (make-branch 2 m1) (make-branch 1 m2)))
(display "#t                = ")(display (balanced? c))(newline)


