#lang racket/base
(require "monad.rkt"

         racket/list racket/contract

         syntax/parse/define

         (for-syntax racket/base))
(provide >>> (contract-out (struct hasket-left ((position (listof exact-nonnegative-integer?)) (exn exn?)))))

;; 数据抽象
;; -----------------------------------------
;; 这个部分是position的内部表现形式，每一个成员代表一次分支，主分支在最后
;; position :: (listof exact-nonnegative-integer?)
(define-syntax-rule (update-position pos proc)
  (struct-copy position pos (value (proc (position-value pos)))))
(define (init) (position '()))
(define (add-branch p)
  (update-position p (lambda (l) (cons 0 l))))
(define (increment p)
  (update-position p (lambda (l) (list-update l 0 add1))))

;; 这个部分中的position是提供给用户使用的表现形式，每一个成员代表一次分支，主分支在第一个
(struct hasket-left (position exn) #:constructor-name make-hasket-left)
(define (errorR->hasket-exn e)
  (make-hasket-left (reverse (position-value (errorR-position e))) (errorR-exception e)))
;; ------------------------------------------

(define-syntax-parser pipeline
  ((_ value:expr ((~datum $) body:expr ...) next:expr ...)
   #'(lambda (p)
       (define new ((pipeline value next ...) (increment p)))
       (cond ((errorR? new) ((pipeline (unitP (errorR->hasket-exn new)) body ...) (add-branch p)))
             (else new))))
  ((_ value:expr first:expr next:expr ...)
   #'(bindP (bindP value first) (lambda (new) (lambda (p) ((pipeline (unitP new) next ...) (increment p))))))
  ((_ value:expr) #'value))

;; 最后的结果会被解包
(define-syntax-parse-rule (>>> val:expr step:expr ...)
  (let ((result ((pipeline (unitP val) step ...) (add-branch (init)))))
    (cond ((errorR? result) (errorR->hasket-exn result))
          (else (unitR-value result)))))

(module+ test
  (require rackunit)

  (check-true (zero? (>>> 0)))
  (check-true (zero? (>>> 0 unitP)))
  (check-true (hasket-left? (>>> 0 (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))))
  (check-true (>>> 0 ($ (lambda (v) (unitP (hasket-left? v)))) (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks))))))
  (>>> 0
       ($ (lambda (v) (unitP (check-equal? '(1) (hasket-left-position v)))))
       (lambda (v) (errorP (exn (format "~a" v) (current-continuation-marks)))))
  )
