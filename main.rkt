#lang racket/base

(module reader syntax/module-reader hasket)

(require "base/main.rkt"
         "amb/main.rkt"
         )
(provide (all-from-out "base/main.rkt")
         (all-from-out "amb/main.rkt")
         )
