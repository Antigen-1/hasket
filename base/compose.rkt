#lang racket/base
(require syntax/parse/define "curry.rkt" (for-syntax racket/base))
(provide (rename-out (n:#%app #%app)))

(begin-for-syntax
  (define-syntax-class first-procedure
    #:description "函数组合中的第一个函数"
    (pattern second:id)
    (pattern ((~literal lambda) p ...))
    (pattern ((~literal lambda/curry/match) p ...))
    (pattern ((~literal curry/n) p ...))
    ))
(define-syntax-parser n:#%app
  ((_ . (then:expr ... . first:first-procedure))
   #'(compose1 then ... first))
  ((_ . (proc:expr argument:expr ...))
   #'(#%app proc argument ...)))

(module+ test
  (require rackunit)
  (check-true ((n:#%app zero? . sub1) 1))
  (check-true ((n:#%app zero? . (lambda (v) (sub1 v))) 1))
  (check-true ((n:#%app zero? . (lambda/curry/match ((v) (sub1 v)))) 1))
  (check-true ((n:#%app zero? sub1 . add1) 0))
  (check-true ((n:#%app zero? sub1 . (curry/n + 1)) 1))
  )
