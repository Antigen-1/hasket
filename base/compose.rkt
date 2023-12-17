#lang racket/base
(require syntax/parse/define "curry.rkt" (for-syntax racket/base))
(provide (rename-out (n:#%app #%app)))

(begin-for-syntax
  (define-syntax-class second-procedure
    #:description "函数组合中的第二个函数"
    (pattern second:id)
    (pattern ((~literal lambda) p ...))
    (pattern ((~literal lambda/curry/match) p ...))
    ))
(define-syntax-parser n:#%app
  ((_ . (first:expr . second:second-procedure))
   #'(lambda (v) (first (second v))))
  ((_ . (proc:expr argument:expr ...))
   #'(#%app proc argument ...)))

(module+ test
  (require rackunit)
  (check-true ((n:#%app zero? . sub1) 1))
  (check-true ((n:#%app zero? . (lambda (v) (sub1 v))) 1))
  (check-true ((n:#%app zero? . (lambda/curry/match ((v) (sub1 v)))) 1))
  )
