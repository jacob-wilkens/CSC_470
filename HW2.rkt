;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname HW2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;1.21 First Attempt
;(define -help
;  (lambda (index s1 s2)
;        (if (= index 0)
;            '()
;        (cons (append (cdr s2) (list(car s1)))
;              (cons (cons (car s1) (list(car s2))) (-help (- index 1)(cdr s1) s2))))))
;(define product
;  (lambda (s1 s2)
;    (-help (length s1) s1 s2)))
;
;(product '(a b c) '(x y))

;1.21 Second Attempt
(define iteration
  (lambda (char lst)
  (if (null? lst)
    '()
    (cons (list char (car lst))
          (iteration char (cdr lst))))))

(define product
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      (else (append (iteration (car s1) s2) (product (cdr s1) s2))))))

(product '(a b c) '(x y z))

;1.22
(define filter-in
  (lambda (pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter-in pred (cdr lst))))
        (else (filter-in pred (cdr lst))))))
        
(filter-in number? '(a 2 (1 3) b 7))
(filter-in symbol? '(a (b c) 17 foo))

;1.23 First Attempt
;(define list-index
;  (lambda (pred lst)
;    (cond
;      ((null? lst) #f)
;      ((pred (car lst))
;           (+ 0))
;      (else (+ 1 (list-index pred (cdr lst)))))))
;
;(list-index number? '(a 2 (1 3) b 7))
;(list-index symbol? '(a (b c) 17 foo))
;(list-index symbol? '(1 2 (a b) 3))

;1.23 Second Attempt
(define -h
  (lambda (index lst pred)
    (cond
    ((null? lst) #f)
    ((pred (car lst))
     index)
    (else (-h (+ index 1) (cdr lst) pred)))))

(define list-index
  (lambda (pred lst)
   (-h 0 lst pred)))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))
