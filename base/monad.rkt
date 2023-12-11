#lang racket/base
(require "../private/monad.rkt" racket/function)
(provide (rename-out (n:mapP mapP))
         (except-out (all-from-out "../private/monad.rkt") mapP))

(define n:mapP (curry mapP))
