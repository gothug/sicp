(load "../lib/interpreter.scm")
(load "../lib/interpreter-analyzed-amb.scm")
(load "../lib/util.scm")

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (ambeval-preload exp)
  (ambeval exp
           the-global-environment
           (lambda (val fail)
             (user-print exp))
           (lambda ()
             (announce-output "Failed while preloading"))))

(define (driver-loop)
  (ambeval-preload '(define (require p) (if (not p) (amb))))
  (ambeval-preload
   '(define (an-element-of items)
      (require (not (null? items)))
      (amb (car items) (an-element-of (cdr items)))))

  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (newline)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define the-global-environment (setup-environment))
(driver-loop)
