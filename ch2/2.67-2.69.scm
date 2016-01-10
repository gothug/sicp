(load "../lib/util.scm")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

; -- 2.67

; Solution
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message-encoded '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(define sample-message-decoded '(a d a b b c a))

; Tests
(run-tests-header)

(test "decode")
(should-be (decode sample-message sample-tree) sample-message-decoded)

; -- 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Solution
(define (encode-symbol symbol tree)
  (define (loop bits cur-branch)
    (if (leaf? cur-branch)
        bits
        (if (memq symbol (symbols cur-branch))
            (if (memq symbol (symbols (left-branch cur-branch)))
                (loop (append bits '(0)) (left-branch cur-branch))
                (loop (append bits '(1)) (right-branch cur-branch)))
            (error "symbol not in tree" symbol))))

  (loop '() tree))

; Tests
(run-tests-header)

(test "encode")
(should-be (encode sample-message-decoded sample-tree) sample-message-encoded)

; -- 2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Solution
(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set
                         (make-code-tree
                          (car leaf-set)
                          (cadr leaf-set))
                         (cddr leaf-set)))))

; Tests
(generate-huffman-tree (list '(A 4) '(B 2) '(D 1) '(C 1)))



