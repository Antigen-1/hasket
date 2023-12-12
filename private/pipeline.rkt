#lang racket/base
(require "monad.rkt" "position.rkt"

         syntax/parse/define

         (for-syntax racket/base))
(provide >>> pipeline)

(define-syntax-parser pipeline
  ((_ value:expr ((~datum $) body:expr ...) next:expr ...)
   #'(lambda (p)
       (define new ((pipeline value next ...) (increment p)))
       (cond ((errorR? new) ((pipeline (unitP (flip-errorR new)) body ...) (add-branch p)))
             (else new))))
  ((_ value:expr first:expr next:expr ...)
   #'(bindP (bindP value first) (lambda (new) (lambda (p) ((pipeline (unitP new) next ...) (increment p))))))
  ((_ value:expr) #'value))

;; 最后的结果会被解包
(define-syntax-parse-rule (>>> val:expr catch-or-step:expr ...)
  (let ((result ((pipeline (unitP val) catch-or-step ...) (init))))
    (cond ((errorR? result) (flip-errorR result))
          (else (unitR-value result)))))

(module+ test
  (require rackunit)

  (check-true (zero? (>>> 0)))
  (check-true (zero? (>>> 0 unitP)))
  (check-true (errorR? (>>> 0 (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))))
  (check-true (>>> 0 ($ (lambda (v) (unitP (errorR? v)))) (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks))))))
  (>>> 0
       ($ (lambda (v) (unitP (check-equal? '(1) (at-position (errorR-value v))))))
       (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))
  )
