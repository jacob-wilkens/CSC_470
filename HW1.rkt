(define duple
  (lambda (n x)
    (if (= n 0)
      '()
      (cons x (duple (- n 1) x)))))

(define invert
  (lambda (lst)
  (if (null? lst)
      lst
      (cons(list(car(cdr(car lst))) (car(car lst)))
            (invert (cdr lst))))))

(define down
  (lambda (lst)
    (if(null? lst)
       lst
       (cons(list(car lst)) (down (cdr lst))))))

(duple 2 3)
(invert '((a 1) (a 2) (1 b) (2 b)))
(down '(1 2 3))
