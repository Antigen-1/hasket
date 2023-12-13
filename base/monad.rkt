#lang racket/base
(require "../private/monad.rkt" racket/function)
(provide (rename-out (n:mapP mapP)
                     (errorP Left)
                     (unitP Right))
         joinP
         (struct-out at)
         (struct-out errorR))

(define n:mapP (curry mapP))

(module+ test
  (require rackunit "pipeline.rkt")

  (>>> #f (lambda _ (n:mapP check-true (unitP #t))))
  (>>> #f (lambda _ ((n:mapP check-true) (unitP #t)))))
