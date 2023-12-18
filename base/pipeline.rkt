#lang racket/base
(require "../private/monad.rkt" "../private/position.rkt"

         syntax/parse/define

         (for-syntax racket/base "../private/optimize.rkt"))
(provide >>> >>>/steps $)

(define-syntax $ #f)
(define-syntax-parser pipeline
  ((_ value:expr ((~literal $) body:expr ...) next:expr ...)
   #'(lambda (p)
       (define new ((pipeline value next ...) (increment p)))
       (cond ((errorR? new) ((pipeline (unitP (flip-errorR new)) body ...) (add-branch p)))
             (else new))))
  ((_ value:expr first:expr next:expr ...)
   #'(bindP (bindP value first) (lambda (new) (lambda (p) ((pipeline (unitP new) next ...) (increment p))))))
  ((_ value:expr) #'value))

(begin-for-syntax
  (define optimize (make-pipeline-optimizer
                    #'>>>
                    #'>>>/steps
                    #'errorP
                    #'unitP
                    #'$)))

;; 最后的结果会被解包
(define-syntax (>>> stx)
  (syntax-parse (optimize stx)
    (((~literal >>>) val:expr catch-or-step:expr ...)
     #'(let ((result ((pipeline (unitP val) catch-or-step ...) (init))))
         (cond ((errorR? result) (flip-errorR result))
               (else (unitR-value result)))))
    (v #'v)))

#|
本来想直接改pipeline的，但pipeline的value是(-> position-value result)
而我想要的composition要求的value是any
|#
(define-syntax (>>>/steps stx)
  (syntax-parse (optimize stx)
    (((~literal >>>/steps) catch-or-step:expr ...)
     #'(lambda (value)
         (pipeline (unitP value) catch-or-step ...)))
    (v #'v)))

(module+ test
  (require rackunit)

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
    (check-true (zero? (cadr d))))
  (let ((a 1))
    (check-true (= 1 (>>> a))))
  (let ((a (>>>/steps)))
    (check-true (zero? (>>> 0 a))))
  (check-true (zero? (>>> 0)))
  (check-true (zero? (>>> 0 unitP)))
  (check-true (errorR? (>>> 0 (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))))
  (check-true (>>> 0 ($ (lambda (v) (unitP (errorR? v)))) (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks))))))
  (>>> 0
       ($ (lambda (v) (unitP (check-equal? '(1) (at-position (errorR-value v))))))
       (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))
  (check-true (zero? (>>> 0 (>>>/steps unitP))))
  )
