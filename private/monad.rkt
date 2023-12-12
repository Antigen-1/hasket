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
         flip-errorR
         unitP
         errorP
         bindP
         resetP
         mapP
         joinP
         )

;; R
(struct (a) errorR ([value : a]))
(struct (a) unitR ([value : a]))
(define-type (Result a b) (U (errorR b) (unitR a)))
(: bindR (All (a b) (-> (Result a b) (-> a (Result a b)) (Result a b))))
(define (bindR value proc)
  (cond ((unitR? value) (proc (unitR-value value)))
        (else value)))

;; P
(struct (a) at ((value : a) (position : Position-Value)) #:type-name At)
(define-type (Position a b) (-> Position-Value (Result a (At b))))
(: unitP (All (a) (-> a (-> Position-Value (unitR a)))))
(define ((unitP a) p) (unitR a))
(: errorP (All (a) (-> a (-> Position-Value (errorR (At a))))))
(define ((errorP e) p) (errorR (at e p)))
(: bindP (-> (Position Any Any) (-> Any (Position Any Any)) (Position Any Any)))
(define ((bindP m k) p) ((inst bindR Any (At Any)) (m p) (lambda (v) ((k v) p))))
(: resetP (All (a b) (-> Position-Value (-> (Position a b) (Position a b)))))
(define (((resetP q) m) p) (m q))

(: liftA (All (a) (-> (-> Position-Value Position-Value) (-> (At a) (At a)))))
(define ((liftA proc) a)
  (struct-copy at a (position (proc (at-position a)))))
(: liftE (All (a) (-> (-> (At a) (At a)) (-> (errorR (At a)) (errorR (At a))))))
(define ((liftE proc) e)
  (struct-copy errorR e (value (proc (errorR-value e)))))
(: flip-errorR (-> (errorR (At Any)) (errorR (At Any))))
(define flip-errorR ((inst liftE Any) ((inst liftA Any) flip)))

#|
Left unit:
(unitM a) `bindM` k = k a
Right unit:
m `bindM` unitM = m
Associative:
m `bindM` (\a -> (k a) `bindM` h) = (m `bindM` (\a -> (k a)) `bindM` h)
|#

(: mapP (-> (-> Any Any) (Position Any Any) (Position Any Any)))
(define (mapP f m)
  (bindP m (lambda (a) (unitP (f a)))))
(: joinP (-> (Position (Position Any Any) Any) (Position Any Any)))
(define (joinP m)
  (bindP m (lambda (a) (cast a (Position Any Any)))))
