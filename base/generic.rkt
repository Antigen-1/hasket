#lang racket/base
(require "../private/generic.rkt"
         racket/function)
(provide Left Right unitL unitS
         joinM (rename-out (n:mapM mapM) (n:bindM bindM))
         gen:monad monad-implement? monad? monad/c
         )

(define n:mapM (curry mapM))
(define n:bindM (curry bindM))
