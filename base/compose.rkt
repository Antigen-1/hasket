#lang racket/base
(require syntax/parse/define (for-syntax racket/base))
(provide (rename-out (n:#%app #%app)))

(define-syntax-parser n:#%app
  ((_ proc:expr argument:expr ...)
   #'(#%app proc argument ...))
  ((_ first:expr . second:expr)
   #'(lambda (v) (first (second v)))))

(module+ test
  (require rackunit)
  (check-true ((n:#%app zero? . sub1) 1)))
