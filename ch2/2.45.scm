; Solution

(define (split op1 op2)
  (define (generic-split painter n)
    (if (= n 0)
	painter
	(let ((smaller (generic-split painter (- n 1))))
	    (op1 painter (op2 smaller smaller)))))
  generic-split)
