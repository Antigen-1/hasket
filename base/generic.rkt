#lang racket/base
(require "../private/generic.rkt"
         racket/function)
(provide Left Right unitL
         bindM joinM (rename-out (n:mapM mapM))
         gen:monad monad-implement? monad? monad/c
         )

(define n:mapM (curry mapM))
