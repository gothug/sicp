; Solution

; a
(define (make-semaphore n)
  (let ((mutex-counter make-mutex)
        (mutex-exceed make-mutex)
        (counter 0))

    (define (acquire)
      (mutex-counter 'acquire)
      (if (< counter n)
          (begin
            (set! counter (+ counter 1))
            (mutex-counter 'release))
          (begin
            (mutex-counter 'release)
            (mutex-exceed 'acquire)
            (acquire))))

    (define (release)
      (mutex-counter 'acquire)
      (if (> counter 0)
          (begin
            (if (= counter n)
                (mutex-exceed 'release))
            (set! counter (- counter 1))))
      (mutex-counter 'release))

    (define (semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    semaphore))

; b
(define (make-semaphore n)
  (define (clear! cell) (set-car! cell false))

  (let ((cell (list false))
        (counter 0))

    (define (acquire)
      (if (test-and-set! cell)
          (acquire)
          (if (< counter n)
              (begin
                (set! counter (+ counter 1))
                (clear! cell))
              (begin
                (clear! cell)
                (acquire)))))

    (define (release)
      (if (test-and-set! cell)
          (release)
          (begin
            (if (> counter 0)
                (set! counter (- counter 1)))
            (clear! cell))))

    (define (semaphore m)
      (cond ((eq? m 'acquire) (acquire))
            ((eq? m 'release) (release))))
    semaphore))
