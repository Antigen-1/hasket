#lang racket/base
(require racket/generic racket/list "monad.rkt")
(provide (rename-out (n:bindP bindP) (n:bindPL bindPL) (n:resetP resetP))
         Left Right
         bindM joinM mapM
         gen:monad monad-implement? monad? monad/c)

;; 数据抽象
;; 方便generic interface的调度
;; 使用了动态特性来于bindP、bindPL兼容
(struct wrapper (result) #:property prop:procedure (struct-field-index result))
(define (n:bindP m p) (wrapper (bindP (wrapper-result m) p)))
(define (n:bindPL m p) (wrapper (bindPL (wrapper-result m) p)))
(define ((n:resetP p) m) (wrapper ((resetP p) (wrapper-result m))))
(define Left (compose1 wrapper errorP))
(define Right (compose1 wrapper unitP))

(define (fallback-joinM m)
  (bindM m (lambda (a) a)))

(define-generics monad
  [bindM monad proc]
  [joinM monad]
  [mapM proc monad]
  #:defined-predicate monad-implement?
  #:fast-defaults ([list? (define bindM bindL)
                          (define mapM map)
                          (define joinM append*)]
                   [wrapper? (define bindM n:bindP)
                             (define (mapM p m) (n:bindP m (lambda (v) (Right (p v)))))]
                   )
  #:fallbacks [(define joinM fallback-joinM)])
