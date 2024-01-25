#lang racket/base
(require racket/generic racket/list racket/stream "monad.rkt")
(provide (rename-out (n:bindP bindP) (n:bindPL bindPL) (n:resetP resetP))
         Left Right unitL unitS
         bindM joinM mapM
         gen:monad monad-implement? monad? monad/c)

;; 数据抽象
;; 方便generic interface的调度
;; 使用了动态特性来与pipeline.rkt兼容
;; resetP、bindP、bindPL、Left和Right的结果可能会作为gen:monad方法的输入，因此使用wrapper封装，顺带作了优化（即避免使用动态特性）
(struct wrapper (result) #:property prop:procedure (struct-field-index result))
(define (n:bindP m p) (wrapper (bindP (wrapper-result m) p)))
(define (n:bindPL m p) (wrapper (bindPL (wrapper-result m) p)))
(define ((n:resetP p) m) (wrapper ((resetP p) (wrapper-result m))))
(define Left (compose1 wrapper errorP))
(define Right (compose1 wrapper unitP))

(define (unitS v) (in-list (list v)))

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
                   [stream? (define (bindM s p) (apply stream-append (stream->list (stream-map p s))))
                            (define mapM stream-map)]
                   [wrapper? (define bindM n:bindP)
                             (define (mapM p m) (n:bindP m (lambda (v) (Right (p v)))))]
                   )
  #:fallbacks [(define joinM fallback-joinM)])
