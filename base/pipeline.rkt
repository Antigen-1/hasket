#lang racket/base
(require "../private/pipeline.rkt" "../private/monad.rkt"

         (for-syntax racket/base "../private/optimize.rkt"))
(provide (rename-out (n:>>> >>>)
                     (n:>>>/steps >>>/steps))
         $)

;; Optimal syntactic forms
(begin-for-syntax (define optimize (make-pipeline-optimizer #'n:>>> #'n:>>>/steps #'>>> #'>>>/steps #'errorP #'unitP #'$)))
(define-syntax (n:>>> stx)
  (optimize stx))
(define-syntax (n:>>>/steps stx)
  (optimize stx))

(module+ test
  (require rackunit (submod ".."))

  (define-namespace-anchor anchor)
  (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
    (check-exn exn:fail:syntax? (lambda () (expand '(>>> #:a))))
    (check-exn exn:fail:syntax? (lambda () (expand '(>>>)))))
  (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
    (define l '(>>> 0 ($ unitP) unitP unitP))
    (displayln "Original:")
    (writeln l)
    (displayln "Expanded:")
    (define d (syntax->datum (time (expand l))))
    (writeln d)
    (check-true (zero? (cadr d))))
  (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
    (define l '(>>> 0 (>>>/steps ($ (>>>/steps unitP)))))
    (displayln "Original:")
    (writeln l)
    (displayln "Expanded:")
    (define d (syntax->datum (time (expand l))))
    (writeln d)
    (check-true (zero? (cadr d)))))
