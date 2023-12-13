#lang racket/base
(require "../private/monad.rkt" "../private/position.rkt"

         syntax/parse/define

         (for-syntax racket/base))
(provide >>> >>>/steps)

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

#|
本来想直接改pipeline的，但pipeline的value是(-> position-value result)
而我想要的composition要求的value是any
|#
(define-syntax-parse-rule (>>>/steps catch-or-step:expr ...)
  (lambda (value)
    (pipeline (unitP value) catch-or-step ...)))

(module+ test
  (require rackunit)

  (check-true (zero? (>>> 0)))
  (check-true (zero? (>>> 0 unitP)))
  (check-true (errorR? (>>> 0 (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))))
  (check-true (>>> 0 ($ (lambda (v) (unitP (errorR? v)))) (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks))))))
  (>>> 0
       ($ (lambda (v) (unitP (check-equal? '(1) (at-position (errorR-value v))))))
       (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))
  (check-true (zero? (>>> 0 (>>>/steps unitP))))
  )
