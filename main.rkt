#lang racket/base

(module reader syntax/module-reader hasket)

(require "private/pipeline.rkt" "private/curry.rkt" "private/compose.rkt"
         "base/monad.rkt")
(provide >>>
         lambda/curry/match
         curry/n
         #%app

         (rename-out
          (unitP Right)
          (errorP Left))

         (struct-out errorR)
         (struct-out at)

         mapP
         joinP

         (all-from-out racket/base))
