(load "../lib/util.scm")
(load "../lib/funprog.scm")

(define make-vect cons)
(define make-segment cons)

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

; Solution
(define outline-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond-painter
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0.5 1) (make-vect 1 0.5)))))

; == wave painter
(define (flip-coords-vertically coords x-axis)
  (map
   (lambda (xy-pair)
     (let ((x (car xy-pair))
           (y (cdr xy-pair)))
       (cons (+ x-axis (- x-axis x)) y)))
   coords))

(define (polyline coords)
  (define (iter head tail segments)
    (if (null? tail)
        segments
        (iter
         (car tail)
         (cdr tail)
         (append segments
                 (list (make-segment
                        (make-vect (car head) (cdr head))
                        (make-vect (car (car tail)) (cdr (car tail)))))))))

  (if (null? coords)
      (list)
      (iter (car coords) (cdr coords) (list))))

; define coords of left and right part of the bottom of the body
(define left-bottom-points
  (list (cons 0.5 0.3)
        (cons 0.4 0)
        (cons 0.3 0)
        (cons 0.4 0.3)
        (cons 0.45 0.6)))

(define right-bottom-points
  (flip-coords-vertically left-bottom-points 0.5))

; define coords of left and right part of the head
(define left-head-points
  (list (cons 0.45 0.7)
        (cons 0.35 0.85)
        (cons 0.45 1)
        (cons 0.5 1)))

(define right-head-points
  (flip-coords-vertically left-head-points 0.5))

; define coords of left and right hand
(define left-hand-points
  (list (cons 0.45 0.6)
        (cons 0.35 0.6)
        (cons 0.25 0.5)
        (cons 0.1 0.7)
        (cons 0.15 0.7)
        (cons 0.25 0.6)
        (cons 0.35 0.7)
        (cons 0.45 0.7)))

(define right-hand-points
  (list (cons 0.55 0.6)
        (cons 0.65 0.6)
        (cons 0.8 0.4)
        (cons 0.85 0.4)
        (cons 0.65 0.7)
        (cons 0.55 0.7)))

(define segments
  (flatmap
   polyline
   (list
    left-bottom-points
    right-bottom-points
    left-head-points
    right-head-points
    left-hand-points
    right-hand-points)))

(define wave-painter (segments->painter segments))
