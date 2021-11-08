;***** Environment Stuff *****
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

(define construct-scope
  (lambda (list-of-2-lists env)
    (map (lambda (2-list) (list (car 2-list) (run-parsed-code (cadr 2-list) env))) list-of-2-lists)))

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

(define no-code-let-env-parser
  (lambda (list-of-2-lists)
    (map (lambda (2-list) (list (car 2-list) (no-parser (cadr 2-list)))) list-of-2-lists)))  

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'local-vars)
       (list 'let-exp
             (no-code-let-env-parser (cadr no-code))
             (no-parser (caddr no-code))))
      ((eq? (car no-code) 'ask)
       (list 'ask-exp
             (boolean-expression-parser (cadr no-code))
             (no-parser (caddr no-code))
             (no-parser (car (reverse no-code)))))
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

(define findVar
  (lambda (lst var)
    (cond
      ((null? lst) '())
      ((eq? (caar lst) var) (cadr(cadar lst)))
      (else (findVar (cdr lst) var)))))

(define findVal
  (lambda (lst var)
        (cond
      ((null? lst) '())
      ((eq? (caar lst) var) (cadar lst))
      (else (findVal (cdr lst) var)))))

(define updatedVars
  (lambda (scopeOrig scope 2lst)
    (cond
      ((null? scope) '())
      ((eq? (cadar scope) #f) (cons (list (caar scope) (findVal scopeOrig (findVar 2lst (caar scope)))) (updatedVars scopeOrig (cdr scope) 2lst)))
      (else (cons (car scope) (updatedVars scopeOrig (cdr scope) 2lst))))))

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
       (let* ((new-scope (construct-scope (cadr parsed-no-code) env))
              (new-scope-with-vars (updatedVars new-scope new-scope (cadr parsed-no-code)))
              (new-env (extend-env new-scope-with-vars env))
              (body (caddr parsed-no-code)))
         (run-parsed-code body new-env))) 
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'ask-exp)
       (if (run-parsed-boolean-code (cadr parsed-no-code) env) 
           (run-parsed-code (caddr parsed-no-code) env)
           (run-parsed-code (cadddr parsed-no-code) env)))
      (else ;this is call lambda code stuff lol woot WHO??
         (let ((function (cadr parsed-no-code)))
            (run-parsed-function-code
             function
             (extend-env
         (let ((list-of-var-names (cdr (cadr (cadr parsed-no-code))))
               (list-of-var-values (map (lambda (packet) (run-parsed-code (car packet) (cadr packet)))
              (map (lambda (x) (list x env)) (caddr parsed-no-code)))))
         (extend-scope
          list-of-var-names
          list-of-var-values
          empty-scope))
         (pop-to-global env))))))))

(define env '((global (age 21))))
(define sample-let-no-code '(local-vars ((a 2) (b 10)) (local-vars ((a 2) (b a) (r a)) (do-mathy-stuff + b r))))
(define parsed-no-code (no-parser sample-let-no-code))
(run-parsed-code parsed-no-code env)
