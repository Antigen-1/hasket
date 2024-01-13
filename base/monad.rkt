#lang racket/base
(require "../private/generic.rkt"
         (except-in "../private/monad.rkt" bindP bindPL resetP)
         racket/function)
(provide Left Right
         bindM joinM (rename-out (n:mapM mapM))
         gen:monad monad-implement? monad? monad/c
         (struct-out at)
         (struct-out errorR))

(define n:mapM (curry mapM))
