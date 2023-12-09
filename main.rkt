#lang racket/base

(module reader syntax/module-reader "./main.rkt")

(require "private/pipeline.rkt" "private/curry.rkt" "private/monad.rkt")
(provide >>>
         lambda/curry/match
         curry/n

         (rename-out
          (unitP Right)
          (errorP Left))

         (struct-out exn:fail:hasket)

         (all-from-out racket/base))
