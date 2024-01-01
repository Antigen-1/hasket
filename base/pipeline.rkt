#lang racket/base
(require "../private/pipeline.rkt" "../private/monad.rkt"

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
  (require rackunit (submod "..") "compose.rkt")

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
  (check (lambda (v) (equal? v (let/cc cc (check cc '(>>> (>>> 0 unitP errorP) unitP))))) '(>>> (>>> 0 unitP errorP unitP) ($ unitP errorP)))
  (check (zero? . cadr) '(>>> 0 ($ unitP) unitP unitP))
  (check (zero? . cadr) '(>>> 0 (>>>/steps ($ (>>>/steps unitP)))))
  (check (zero? . cadr) '(>>> (>>> (>>> 0) unitP (>>>/steps unitP)) unitP)))
