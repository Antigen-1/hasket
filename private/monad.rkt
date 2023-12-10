#lang typed/racket/base/shallow ;; 加速加速！！！
(require "position.rkt")
(provide (struct-out errorR)
         (struct-out unitR)
         flip-errorR
         unitP
         errorP
         bindP
         resetP
         )

;; R
(struct errorR ([exception : exn] [position : Position-Value]))
(struct unitR ([value : Any]))
(define-type Result (U errorR unitR))
(: bindR (-> Result (-> Any Result) Result))
(define (bindR value proc)
  (cond ((unitR? value) (proc (unitR-value value)))
        (else value)))

(: lift (-> (-> Position-Value Position-Value) (-> errorR errorR)))
(define ((lift proc) e)
  (struct-copy errorR e (position (proc (errorR-position e)))))
(: flip-errorR (-> errorR errorR))
(define flip-errorR (lift flip))

;; P
(define-type Position (-> Position-Value Result))
(: unitP (-> Any (-> Position-Value unitR)))
(define ((unitP a) p) (unitR a))
(: errorP (-> exn (-> Position-Value errorR)))
(define ((errorP e) p) (errorR e p))
(: bindP (-> Position (-> Any Position) Position))
(define ((bindP m k) p) (bindR (m p) (lambda (v) ((k v) p))))
(: resetP (-> Position-Value (-> Position Position)))
(define (((resetP q) m) p) (m q))

#|
Left unit:
(unitM a) `bindM` k = k a
Right unit:
m `bindM` unitM = m
Associative:
m `bindM` (\a -> (k a) `bindM` h) = (m `bindM` (\a -> (k a)) `bindM` h)
|#
