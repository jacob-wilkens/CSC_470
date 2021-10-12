(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? varName (caar env)) (cadar env))
      (else (resolve varName (cdr env))))))

(define extend-env
  (lambda (lo-vars lo-vals env)
    (cond
      ((null? lo-vars) env)
      (else (extend-env (cdr lo-vars)
                        (cdr lo-vals)
                        (cons (list (car lo-vars) (car lo-vals)) env))))))
                              
(define do-mathy-stuff-toaster
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '//) (quotient num1 num2))
      ((eq? op '%) (modulo num1 num2))
      ((eq? op '*) (* num1 num2))
      (else #f))))

(define let-part1
  (lambda (c env)
    (cond
      ((null? c) '())
      (else (append (let-part2 (car c) env) (let-part1 (cdr c) env))))))

(define let-begin
  (lambda (c env)
      (run-parsed-code (cadr(caddr c)) (let-part1 (cadr c) env))))

(define let-part2
  (lambda (c env)
    (extend-env (list(car c)) (list(run-parsed-code (cadr c) env)) env)))

; ***** PARSERS *****
(define let-parser
  (lambda (c)
    (cond
      ((null? c) '())
      (else (cons (l (car c)) (let-parser(cdr c)))))))

(define l
  (lambda (c)
    (list (car c) (no-parser (cadr c)))))

(define boolean-expression-parser
  (lambda (boolean-expression)
    (cond
      ((eq? (car boolean-expression) '<)
       (list 'less-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '<=)
       (list 'less-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>)
       (list 'greater-then
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '>=)
       (list 'greater-then-or-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '==)
       (list 'equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      ((eq? (car boolean-expression) '!=)
       (list 'not-equal
             (no-parser (cadr boolean-expression))
             (no-parser (caddr boolean-expression))))
      (else "Not a valid boolean expression"))))

(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      ((eq? (car no-code) 'let)
       (list 'let-exp
             (let-parser (cadr no-code))
             (list 'body (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****
(define run-parsed-boolean-code
  (lambda (parsed-boolean-code env)
    (cond
      ((eq? (car parsed-boolean-code) 'less-then)
       (<
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'less-then-or-equal)
       (<=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then)
       (>
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'greater-then-or-equal)
       (>=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'equal)
       (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env)))
      ((eq? (car parsed-boolean-code) 'not-equal)
       (not (=
        (run-parsed-code (cadr parsed-boolean-code) env)
        (run-parsed-code (caddr parsed-boolean-code) env))))
      (else "Not a legal boolean expression!"))))
       
(define run-parsed-function-code
  (lambda (parsed-no-code-function env)
    (run-parsed-code (cadr (caddr parsed-no-code-function)) env)))
        
(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env) 
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'let-exp)
       (let-begin parsed-no-code '()))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(define s '(ask (< 5 10) (let ((b 2) (c 7)) (do-mathy-stuff + b c)) otherwise 10))
(define p (no-parser s))
;p
;(let-part1 (list (list 'b (list 'num-lit-exp 2)) (list 'c (list 'num-lit-exp 3))) '())
;(let-parser '(let ((b 2) (c 3)) (+ b c)))
(run-parsed-code p env)
;(define x (list 'let-exp (list (list 'b (list 'num-lit-exp 2)) (list 'c (list 'num-lit-exp 3))) (list 'body (list 'math-exp '+ (list 'var-exp 'b) (list 'var-exp 'c)))))
;(let-begin x '())
