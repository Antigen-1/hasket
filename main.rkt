#lang racket/base

(module reader syntax/module-reader hasket)

(require "private/pipeline.rkt" "private/curry.rkt" "private/monad.rkt")
(provide >>>
         lambda/curry/match
         curry/n

         (rename-out
          (unitP Right)
          (errorP Left))

         (struct-out hasket-left)

         (all-from-out racket/base))
