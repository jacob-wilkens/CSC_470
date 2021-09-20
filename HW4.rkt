(define no-parser
  (lambda (no-code)
    (cond
      ((symbol? no-code) (list 'var-exp no-code))
      ((eq? (car no-code) 'function)
       (list 'func-exp
             (append (list 'params) (cadr no-code))
             (list 'body
                   (no-parser (caddr no-code)))))
      (else (list 'call-exp
                  (no-parser (cadr no-code))
                  (no-parser (caddr no-code)))))))

(define parsevar
  (lambda (lst)
    (cadr lst)))

(define parseFunc
  (lambda (lst)
    (append (list 'function) (parseInnerFunc (cdr lst)))))

(define parseInnerFunc
  (lambda (lst)
    (cond
      ((eq? (caar lst) 'params) (cons (cdar lst) (list(parseInnerFunc(list(cadr lst))))))
      (else (cadr(cadr(car lst)))))))

(define no-unparser
  (lambda (code)
    (cond
      ((eq? (car code) 'var-exp) (parsevar code))
      ((eq? (car code) 'func-exp) (parseFunc code))
      (else (append (list 'call) (cons (parseFunc (cadr code)) (list(parsevar (caddr code)))))))))

(define sample-no-code '(call (function (x) x) a))
(no-unparser (no-parser sample-no-code))
