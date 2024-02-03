(module func "../base/main.rkt"
  (require "syntax.rkt" racket/match)
  (provide amb-foldl amb-map amb-andmap amb-ormap amb-filter)

  (match-define (list amb-foldl) (amb-begin (let amb-foldl (lambda (p o l) (if (null? l) o (amb-foldl p (p (car l) o) (cdr l))))) amb-foldl))
  (match-define (list amb-map) (amb-begin (let amb-map (lambda (p l) (if (null? l) null (cons (p (car l)) (amb-map p (cdr l)))))) amb-map))
  (match-define (list amb-andmap) (amb-begin (let amb-andmap (lambda (p l) (if (null? l) #t (if (p (car l)) (amb-andmap p (cdr l)) #f)))) amb-andmap))
  (match-define (list amb-ormap) (amb-begin (let amb-ormap (lambda (p l) (if (null? l) #f (if (p (car l)) #t (amb-ormap p (cdr l)))))) amb-ormap))
  (match-define (list amb-filter) (amb-begin (let amb-filter (lambda (p l) (if (null? l) null (if (p (car l)) (cons (car l) (amb-filter p (cdr l))) (amb-filter p (cdr l)))))) amb-filter))

  (module+ test
    (require rackunit)

    (check-equal? (amb-begin (amb-map add1 '(1 2 3 4))) '((2 3 4 5)))
    (check-equal? (amb-begin (amb-andmap not '(#f #f))) '(#t))
    (check-equal? (amb-begin (amb-ormap not '(#t #f #t))) '(#t))
    (check-equal? (amb-begin (amb-foldl + 0 (list (amb 1 2 3) 4))) '(5 6 7))
    (check-equal? (amb-begin (amb-filter values (list 1 2 #f #t 3))) '((1 2 #t 3)))))
