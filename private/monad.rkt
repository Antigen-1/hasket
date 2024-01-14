#|
注意：
由于这个模块绝大多数都是与untyped racket交互，因此
1.使用typed/racket/base/shallow
2.导出的函数都使用了Any类型（由于缺乏类型推导，type constructor就相当于Any类型）
|#

#lang typed/racket/base/shallow ;; 加速加速！！！
(require "position.rkt")
(provide (struct-out errorR)
         (struct-out unitR)
         (struct-out at)
         unitP
         errorP
         bindP
         bindPL
         resetP
         unitL
         bindL
         )

;; R
(struct (a) errorR ([value : a]))
(struct (a) unitR ([value : a]))
(define-type (Result a b) (U (errorR b) (unitR a)))
(: bindR (All (a b c d) (-> (Result a b) (-> a (Result c d)) (Result c (U b d)))))
(define (bindR value proc)
  (cond ((unitR? value) (proc (unitR-value value)))
        (else value)))
;; 只是长得像bindM
;; proc直接处理errorR
;; unitR直接被返回
;; 其实本可以写成monad，但出于兼容性的考虑，采取了折衷方案
(: bindRL (All (a b c d) (-> (Result a b) (-> (errorR b) (Result c d)) (Result (U a c) d))))
(define (bindRL value proc)
  (cond ((errorR? value) (proc value))
        (else value)))

;; P
(struct (a) at ((value : a) (position : Position-Value)) #:type-name At)
(define-type (Position a b) (-> Position-Value (Result a (At b))))
(: unitP (All (a) (-> a (-> Position-Value (unitR a)))))
(define ((unitP a) p) (unitR a))
(: errorP (All (a) (-> a (-> Position-Value (errorR (At a))))))
(define ((errorP e) p) (errorR (at e p)))
(: bindP (All (a b c d) (-> (Position a b) (-> a (Position c d)) (Position c (U b d)))))
(define ((bindP m k) p) ((inst bindR a (At b) c (At d)) (m p) (lambda (v) ((k v) p))))
;; 只是长得像bindM，bindP的bindRL版
(: bindPL (All (a b c d) (-> (Position a b) (-> (errorR (At b)) (Position c d)) (Position (U a c) d))))
(define ((bindPL m k) p) ((inst bindRL a (At b) c (At d)) (m p) (lambda (v) ((k ((inst flip-errorR b) v)) p))))
(: resetP (All (a b) (-> Position-Value (-> (Position a b) (Position a b)))))
(define (((resetP q) m) p) (m q))

(: liftA (All (a) (-> (-> Position-Value Position-Value) (-> (At a) (At a)))))
(define ((liftA proc) a)
  (struct-copy at a (position (proc (at-position a)))))
(: liftE (All (a) (-> (-> a a) (-> (errorR a) (errorR a)))))
(define ((liftE proc) e)
  (struct-copy errorR e (value (proc (errorR-value e)))))
(: flip-errorR (All (a) (-> (errorR (At a)) (errorR (At a)))))
(define (flip-errorR e)
  (((inst liftE (At a)) ((inst liftA a) flip)) e))

;; L
(: unitL (All (a) (-> a (Listof a))))
(define unitL list)
(: bindL (All (a b) (-> (Listof a) (-> a (Listof b)) (Listof b))))
(define (bindL l p)
  (apply append (map p l)))

#|
Left unit:
(unitM a) `bindM` k = k a
Right unit:
m `bindM` unitM = m
Associative:
m `bindM` (\a -> (k a) `bindM` h) = (m `bindM` (\a -> (k a)) `bindM` h)
|#
