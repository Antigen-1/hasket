#lang racket/base
(require "monad.rkt" "position.rkt"

         syntax/parse/define

         (for-syntax racket/base))
(provide >>> >>>/steps $)

(define-syntax ($ stx)
  (raise-syntax-error #f "Used out of pipelines" stx))
(define-syntax-parser pipeline
  ((_ value:expr ((~literal $) body:expr) next:expr)
   #'(bindPL (pipeline value next) body))
  ((_ value:expr first:expr)
   #'(bindP value first))
  ((_ value:expr) #'value))
(define-syntax-parser pipeline/location
  ((_ value:expr ((~literal $) body:expr ...) next:expr ...)
   #'(pipeline value
               ($ (lambda (val) (compose1 (pipeline/location (unitP val) body ...) add-branch)))
               (lambda (val) (compose1 (pipeline/location (unitP val) next ...) increment))))
  ((_ value:expr first:expr next:expr ...)
   #'(pipeline (pipeline value first)
               (lambda (val) (compose1 (pipeline/location (unitP val) next ...) increment))))
  ((_ value:expr) #'(pipeline value)))

;; 最后的结果会被解包
(define default-start (init))
(define-syntax (>>> stx)
  (syntax-parse stx
    ((_ val:expr catch-or-step:expr ...)
     #'(unitR-value
        ((bindPL (pipeline/location (unitP val) catch-or-step ...) unitP)
         default-start)))))

#|
本来想直接改pipeline的，但pipeline的value是(-> position-value result)
而我想要的composition要求的value是any
|#
(define-syntax (>>>/steps stx)
  (syntax-parse stx
    ((_ catch-or-step:expr ...)
     #'(lambda (value)
         (pipeline/location (unitP value) catch-or-step ...)))
    ))

(module+ test
  (require rackunit)

  (define-namespace-anchor anchor)
  (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
    (check-exn exn:fail:syntax? (lambda () (expand '($)))))

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
  (>>> 0
       ($ (lambda (v) (unitP (check-equal? (at-position (errorR-value v)) '(1 0)))))
       ($ errorP)
       errorP)
  (check-true (zero? (>>> 0 (>>>/steps unitP))))
  )
