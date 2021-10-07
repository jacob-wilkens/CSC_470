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
; ***** BOOLS *******
(define bool
  (lambda (q exp1 exp2 env)
    (cond
      ((= (bool-exp q (run-parsed-code exp1 env) (run-parsed-code exp2 env)) 1) 1)
      (else 0))))

(define eval-bool
  (lambda (q exp1 exp2)
    (cond
      ((q exp1 exp2) 1)
      (else 0))))

(define bool-exp
  (lambda (q exp1 exp2)
    (cond
      ((eq? q '=)  (eval-bool = exp1 exp2))
      ((eq? q '>=) (eval-bool >= exp1 exp2))
      ((eq? q '>) (eval-bool > exp1 exp2))
      ((eq? q '<) (eval-bool < exp1 exp2))
      ((eq? q '<=) (eval-bool <= exp1 exp2))
      (else #f))))

; ***** PARSERS *****
(define no-code-function-parser
  (lambda (no-code-function)
    (list 'func-exp
             (append (list 'params) (cadr no-code-function))
             (list 'body
                   (no-parser (caddr no-code-function))))))
(define bool-parser
  (lambda (bool-code)
    (list 'ask-exp
             (caadr bool-code)
             (no-parser (cadr(cadr bool-code)))
             (no-parser (caddr(cadr bool-code)))
             (no-parser (caddr bool-code))
             (no-parser (car (reverse bool-code))))))
 
(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'ask)
       (bool-parser no-code))
      (else (list 'call-exp
                  (no-code-function-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))

; ***** Interpreters *****
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
       (if (= (bool (cadr  parsed-no-code) (caddr parsed-no-code) (cadddr parsed-no-code)  env) 1)
           (run-parsed-code (cadr(reverse parsed-no-code)) env)
           (run-parsed-code (car(reverse parsed-no-code)) env)))
      (else
         (run-parsed-function-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(define s '(call (function (x y) (ask (>= 10 10) (do-mathy-stuff + x y) otherwise 5)) (do-mathy-stuff * a b) 15))
(define p (no-parser s))
(run-parsed-code p env)
