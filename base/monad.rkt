#lang racket/base
(require "../private/monad.rkt" "../private/curry.rkt")
(provide mapP joinP)

(define mapP
  (lambda/curry/match
   #:name mapP
   ((f m) (bindP m (lambda (a) (unitP (f a)))))))
(define (joinP z)
  (bindP z (lambda (m) m)))
