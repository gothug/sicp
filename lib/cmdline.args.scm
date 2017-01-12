(define command-line-args '())

(define parse-argument-list
  (lambda (arg-list)
    (set! command-line-args
      (if (null? arg-list)
          (list)
          (cdr arg-list)))))

(set-command-line-parser! "args" parse-argument-list)
