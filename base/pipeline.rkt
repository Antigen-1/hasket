#lang racket/base
(require "../private/pipeline.rkt" "../private/monad.rkt" "../private/position.rkt"

         (for-syntax racket/base "../private/optimize.rkt"))
(provide (rename-out (n:>>> >>>)
                     (n:>>>/steps >>>/steps))
         $)

;; Optimal syntactic forms
(begin-for-syntax (define optimize (make-pipeline-optimizer #'n:>>> #'n:>>>/steps #'>>> #'>>>/steps #'(resetP (init)) #'errorP #'unitP #'$)))
(define-syntax (n:>>> stx)
  (optimize stx))
(define-syntax (n:>>>/steps stx)
  (optimize stx))

(module+ test
  (require rackunit (submod "..") "compose.rkt" (rename-in (only-in "../private/pipeline.rkt" >>> >>>/steps) (>>> o:>>>) (>>>/steps o:>>>/steps)))

  (define-namespace-anchor anchor)

  (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
    (check-exn exn:fail:syntax? (lambda () (expand '(>>> #:a))))
    (check-exn exn:fail:syntax? (lambda () (expand '(>>>)))))

  (define (check pred form)
    (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
      (displayln "Original:")
      (writeln form)
      (displayln "Expanded:")
      (define d (syntax->datum (time (expand form))))
      (writeln d)
      (check-true (pred d))))
  (define (check-equal form1 form2)
    (check (lambda (v) (equal? v (let/cc cc (check cc form1)))) form2))
  (check-equal '(>>> (>>> 0 unitP errorP) unitP) '(>>> (>>> 0 unitP errorP unitP) ($ unitP errorP)))
  (check-equal '(>>> 0 (>>>/steps ($) unitP errorP) errorP unitP) '(o:>>> 0 (o:>>>/steps ($) unitP errorP) errorP))
  (check-equal '(>>> 0 (>>>/steps ($) unitP errorP)) '(o:>>> 0 ($) unitP errorP))
  (check-equal '(>>> 0 ($ unitP) unitP unitP) '(quote 0))
  (check-equal '(>>> 0 (>>>/steps ($ (>>>/steps unitP)))) '(quote 0))
  (check-equal '(>>> (>>> (>>> 0) unitP (>>>/steps unitP)) unitP) '(quote 0)))
