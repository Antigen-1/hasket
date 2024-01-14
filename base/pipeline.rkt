#lang racket/base
(require "../private/pipeline.rkt" "../private/generic.rkt" "../private/position.rkt"

         (prefix-in n: "compose.rkt")

         (for-syntax racket/base "../private/optimize.rkt"))
(provide (rename-out (n:>>> >>>)
                     (n:>>>/steps >>>/steps))
         $)

;; Optimal syntactic forms
(begin-for-syntax (define optimize (make-pipeline-optimizer #'n:>>> #'n:>>>/steps #'>>> #'>>>/steps #'(resetP (init)) #'Left #'Right #'$ #'n:#%app #'lambda)))
(define-syntax (n:>>> stx)
  (optimize stx))
(define-syntax (n:>>>/steps stx)
  (optimize stx))

(module+ test
  (require rackunit (submod "..") (rename-in (only-in "../private/pipeline.rkt" >>> >>>/steps) (>>> o:>>>) (>>>/steps o:>>>/steps)))

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
  (check-equal '(>>> (>>> 0 Right Left) Right) '(>>> (>>> 0 Right Left Right) ($ Right Left)))
  (check-equal '(>>> 0 (>>>/steps ($) Right Left) Left Right) '(o:>>> 0 (o:>>>/steps ($) Right Left) Left))
  (check-equal '(>>> 0 (>>>/steps ($) Right Left)) '(o:>>> 0 ($) Right Left))
  (check-equal '(>>> 0 ($ Right) Right Right) '(quote 0))
  (check-equal '(>>> 0 (>>>/steps ($ (>>>/steps Right)))) '(quote 0))
  (check-equal '(>>> (>>> (>>> 0) Right (>>>/steps Right)) Right) '(quote 0)))
