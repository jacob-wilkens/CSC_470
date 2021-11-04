(define resolve
  (lambda (varName env)
    (cond
      ((null? env) #f)
      ((eq? (caar env) 'global) (resolve-scope varName (cdar env)))
      (else (let ((answer (resolve-scope varName (car env))))
              (if (eq? answer #f)
                  (resolve varName (cdr env))
                  answer))))))

(define resolve-scope
  (lambda (varName scope)
    (cond
      ((null? scope) #f)
      ((eq? varName (caar scope)) (cadar scope))
      (else (resolve-scope varName (cdr scope))))))

(define empty-scope '())

(define extend-scope
  (lambda (lo-vars lo-vals scope)
    (cond
      ((null? lo-vars) scope)
      (else (extend-scope (cdr lo-vars)
                          (cdr lo-vals)
                          (cons (list (car lo-vars) (car lo-vals)) scope))))))
(define extend-env
  (lambda (scope env)
    (cons scope env)))

(define pop-to-global
  (lambda (env)
    (if (eq? (caar env) 'global)
        env
        (pop-to-global (cdr env)))))

(define env-let-get-var-names
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (caar lst) (env-let-get-var-names (cdr lst))))))

(define env-let-get-var-values
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (cadar lst) (env-let-get-var-values (cdr lst))))))

(define env-let-mapper
  (lambda (lst)
    (list (env-let-get-var-names lst)
          (env-let-get-var-values lst))))

(define addScope
  (lambda (scope env)
    (cons scope env)))

(define resolveSymbol
  (lambda (lst env)
    (cond
      ((null? lst) '())
      ((number? (cadar lst)) (cons (list (caar lst)(cadar lst)) (resolveSymbol (cdr lst) env)))
      (else (cons (list (caar lst) (resolve (cadar lst) env)) (resolveSymbol (cdr lst) env)) ))))

; ***** PARSERS *****
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
      ((eq? (car no-code) 'local-vars)
       (list 'let-exp
             (cadr no-code)
             (no-parser (caddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

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
    (let ((body-of-function (cadr (caddr parsed-no-code-function))))
      (run-parsed-code body-of-function env))))

(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'let-exp)
       (let* ((var-2-lists (env-let-mapper (cadr parsed-no-code)))
              (new-scope (extend-scope (car var-2-lists) (cadr var-2-lists) empty-scope))
              (resolvedNewScope (resolveSymbol new-scope env) )
              (updatedEnv (addScope resolvedNewScope env))
              (body (caddr parsed-no-code)))
         (run-parsed-code body updatedEnv)))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env)
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (extend-scope
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet)))
              (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         empty-scope)
         (pop-to-global env)))))))

(define let-sample '(call (function (a) (local-vars ((a 7) (b 3)) (local-vars ((c 4) (d 5) (e a)) (do-mathy-stuff + e b))) ) 25))
(define parsed-let-sample (no-parser let-sample))
(define env '((global (age 21) (a 7) (b 5) (c 23))))
(run-parsed-code parsed-let-sample env)
