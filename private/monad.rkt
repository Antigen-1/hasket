#lang typed/racket/base ;; 加速加速！！！
(provide (struct-out errorR)
         (struct-out unitR)
         (struct-out position)
         unitP
         errorP
         bindP
         resetP
         )

(struct errorR ([exception : exn] [position : position]))
(struct unitR ([value : Any]))
(define-type Result #;R (U errorR unitR))
(: bindR (-> Result (-> Any Result) Result))
(define (bindR value proc)
  (cond ((unitR? value) (proc (unitR-value value)))
        (else value)))

(define-type Position #;P (-> position Result))
(struct position ([value : Any]))
(: unitP (-> Any (-> position unitR)))
(define ((unitP a) p) (unitR a))
(: errorP (-> exn (-> position errorR)))
(define ((errorP e) p) (errorR e p))
(: bindP (-> Position (-> Any Position) Position))
(define ((bindP m k) p) (bindR (m p) (lambda (v) ((k v) p))))
(: resetP (-> position (-> Position Position)))
(define (((resetP q) m) p) (m q))

#|
Left unit:
(unitM a) `bindM` k = k a
Right unit:
m `bindM` unitM = m
Associative:
m `bindM` (\a -> (k a) `bindM` h) = (m `bindM` (\a -> (k a)) `bindM` h)
|#
