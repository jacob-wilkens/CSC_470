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

(define bool
  (lambda (q num1 num2 exp1 exp2 env)
    (cond
      ((eq? (bool-exp q (run-parsed-code num1 env) (run-parsed-code num2 env)) "1") (run-parsed-code exp1 env))
      (else (run-parsed-code exp2 env)))))

(define eval-bool
  (lambda (q num1 num2)
    (cond
      ((q num1 num2) "1")
      (else "0"))))

(define bool-exp
  (lambda (q num1 num2)
    (cond
      ((eq? q '=)  (eval-bool eq? num1 num2))
      ((eq? q '>=) (eval-bool >= num1 num2))
      ((eq? q '>) (eval-bool > num1 num2))
      ((eq? q '<) (eval-bool < num1 num2))
      ((eq? q '<=) (eval-bool <= num1 num2))
      (else #f))))
                              
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

(define no-parser
  (lambda (no-code)
    (cond
      ((number? no-code) (list 'num-lit-exp no-code))
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'if)
       (list 'bool-exp
             (cadr no-code)
             (no-parser (caddr no-code)) ;first no expression
             (no-parser (cadddr no-code)) ;second no expression
             (no-parser (cadr(cdddr no-code))) ;first no expression
             (no-parser (caddr(cdddr t))))) ; second no expression
      ((eq? (car no-code) 'do-mathy-stuff)
       (list 'math-exp (cadr no-code) (no-parser (caddr no-code)) (no-parser (cadddr no-code))))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-parser (cadr no-code))
                  (map no-parser (cddr no-code)))))))
    
(define run-parsed-code
  (lambda (parsed-no-code env)
    (cond
      ((eq? (car parsed-no-code) 'num-lit-exp)
       (cadr parsed-no-code))
      ((eq? (car parsed-no-code) 'var-exp)
       (resolve (cadr parsed-no-code) env))
      ((eq? (car parsed-no-code) 'bool-exp)
      (bool
       (cadr parsed-no-code) ;comparison symbol
       (caddr parsed-no-code) ;first no expression
       (cadddr parsed-no-code) ;second no expression
       (cadr(cdddr parsed-no-code)) ;first no expression
       (caddr(cdddr parsed-no-code)) ;second no expression
       env))
      ((eq? (car parsed-no-code) 'math-exp)
       (do-mathy-stuff-toaster
        (cadr parsed-no-code)
        (run-parsed-code (caddr parsed-no-code) env)
        (run-parsed-code (cadddr parsed-no-code) env)))
      ((eq? (car parsed-no-code) 'func-exp)
       (run-parsed-code (cadr (caddr parsed-no-code)) env))
      (else
       (run-parsed-code
        (cadr parsed-no-code)
        (extend-env
         (cdr (cadr (cadr parsed-no-code)))
         (map (lambda (packet) (run-parsed-code (car packet) (cadr packet))) (map (lambda (x) (list x env)) (caddr parsed-no-code)))
         env))))))

(define env '((age 21) (a 7) (b 5) (c 23)))
(define t '(if > a b (call (function (x y) (do-mathy-stuff + x y)) (do-mathy-stuff * a b) 15) (call (function (x y) (do-mathy-stuff + x y)) (do-mathy-stuff * 12 12) 12)))
(define x (no-parser t))
(run-parsed-code x env)
