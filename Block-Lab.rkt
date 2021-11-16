; A block expression can be a number
; A block expression can be a number, symbol, or another block expression
; A block expression can be a symbol, and two additional block statements
; A block expression can be a symbol

;1.Write a block-exp that contains exactly 8 numbers
(define block-exp-example '(a (b 1 2) (c (d 3 4) (e (f 5 6) (g 7 8)))))

;2. Write a scheme function called count-block-exp that takes a block-exp as a parameter and boils down to the sum of all of the numbers in the block-exp
(define blockSum
  (lambda (b-exp)
    (cond
      ((null? b-exp) 0)
      ((number? b-exp) b-exp)
      ((symbol? b-exp) 0)
      (else (-helperSum b-exp)))))

(define -helperSum
  (lambda (b-exp)
    (cond
      ((null? b-exp) 0)
      ((number? (car b-exp)) (+ (blockSum (car b-exp)) (blockSum (cdr b-exp))))
      ((symbol? (car b-exp)) (+ 0 (blockSum (cdr b-exp))))
      ((list? b-exp) (+ (blockSum (car b-exp)) (blockSum (cdr b-exp))))
      (else (+ (blockSum (car b-exp)) (blockSum (cdr b-exp)))))))

(blockSum block-exp-example)

;3. Write a scheme function called collect-symbols that takes a block-exp as a parameter and returns a list containing all of the symbols found in the block-exp
(define blockCount
  (lambda (b-exp)
    (cond
      ((null? b-exp) '())
      ((number? b-exp) '())
      ((symbol? b-exp) b-exp)
      (else (-helperSymbol b-exp)))))

(define -helperSymbol
  (lambda (b-exp)
    (cond
      ((null? b-exp) '())
      ((number? (car b-exp)) (append '() (-helperSymbol (cdr b-exp))))
      ((symbol? (car b-exp)) (append (list (car b-exp)) (-helperSymbol (cdr b-exp))))
      ((list? b-exp) (append (-helperSymbol (car b-exp)) (-helperSymbol (cdr b-exp))))
      (else (append (-helperSymbol (car b-exp)) (-helperSymbol (cdr b-exp)))))))

(blockCount block-exp-example)
