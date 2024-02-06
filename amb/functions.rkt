(module func "../base/main.rkt"
  (require "syntax.rkt" racket/match)
  (provide (all-defined-out))

  (match-define (list amb-foldl) (amb-begin (let amb-foldl (lambda (p o l) (if (null? l) o (amb-foldl p (p (car l) o) (cdr l))))) amb-foldl))
  (match-define (list amb-foldr) (amb-begin (let amb-foldr (lambda (p o l) (if (null? l) o (p (car l) (amb-foldr p o (cdr l)))))) amb-foldr))
  (match-define (list amb-map) (amb-begin (lambda (p l) (amb-foldr (lambda (v r) (cons (p v) r)) null l))))
  (match-define (list amb-andmap) (amb-begin (let amb-andmap (lambda (p l) (if (null? l) #t (if (p (car l)) (amb-andmap p (cdr l)) #f)))) amb-andmap))
  (match-define (list amb-ormap) (amb-begin (let amb-ormap (lambda (p l) (if (null? l) #f (if (p (car l)) #t (amb-ormap p (cdr l)))))) amb-ormap))
  (match-define (list amb-filtermap) (amb-begin (lambda (p l) (amb-foldr (lambda (v r) (let result (p v)) (if result (cons result r) r)) null l))))
  (match-define (list amb-filter) (amb-begin (lambda (p l) (amb-foldr (lambda (v r) (if (p v) (cons v r) r)) null l))))

  (module+ test
    (require rackunit (only-in "abstract.rkt" amb))

    (check-equal? (amb-begin (amb-map add1 '(1 2 3 4))) '((2 3 4 5)))
    (check-equal? (amb-begin (amb-andmap not '(#f #f))) '(#t))
    (check-equal? (amb-begin (amb-ormap not '(#t #f #t))) '(#t))
    (check-equal? (amb-begin (amb-foldl + 0 (list (amb 1 2 3) 4))) '(5 6 7))
    (check-equal? (amb-begin (amb-foldr cons null (list (amb 1 2) 3 4 5))) '((1 3 4 5) (2 3 4 5)))
    (check-equal? (amb-begin (amb-filtermap (lambda (a) (if (odd? a) a #f)) '(1 2 3 4 5 6 7 8 9 10))) '((1 3 5 7 9)))
    (check-equal? (amb-begin (amb-filter not (list 1 2 #f #t 3))) '((#f)))))
