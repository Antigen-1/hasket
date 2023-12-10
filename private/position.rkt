#lang typed/racket/base/shallow
(provide Position-Value
         init
         add-branch
         increment
         flip)

;; 数据抽象
;; position的内部表现形式：每一个成员代表一次分支，主分支在最后
;; position的用户使用形式：每一个成员代表一次分支，主分支在最前

(define-type Position-Value (Listof Exact-Nonnegative-Integer))
(: lift (-> (-> (Listof Exact-Nonnegative-Integer) (Listof Exact-Nonnegative-Integer)) (-> Position-Value Position-Value)))
(define ((lift proc) pos) (proc pos))

(: init (-> Position-Value))
(define (init) '(0))

(: add-branch (-> Position-Value Position-Value))
(define add-branch
  (lift (lambda (l) (cons 0 l))))
(: increment (-> Position-Value Position-Value))
(define increment
  (lift (lambda (l) (cons (add1 (car l)) (cdr l)))))
(: flip (-> Position-Value Position-Value))
(define flip
  (lift reverse))
