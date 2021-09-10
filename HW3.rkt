;1.27
(define flatten
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
      (else (append (list(car lst))(flatten (cdr lst)))))))

(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))

;1.28
(define merge
  (lambda (s1 s2)
    (cond
       ((and (null? s1) (null? s2))'())
       ((null? s1) s2)
       ((null? s2) s1)
       (else
        (cond
          ((< (car s1) (car s2)) (cons (car s1) (merge (cdr s1) s2)))
          (else (cons (car s2) (merge s1 (cdr s2)))))))))
          
(merge '(1 4) '(1 2 8))
(merge '(35 62 81 90 91) '(3 83 85 90))
