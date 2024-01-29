(module procedure "../base/main.rkt"
  (require (submod racket/performance-hint begin-encourage-inline) racket/list)
  (provide wrap call-all amb-apply n:procedure?)
  ;; Inline instructions
  (begin-encourage-inline
    (struct wrapper (procedure) #:constructor-name wrap)

    (define (call-all proc args)
      (define call (curry/n apply 2))
      (define (call-primitive procedure args)
        (bindM (apply cartesian-product args)
               (compose1 unitL (call procedure))))
      (define (call-wrapped procedure args)
        (bindM (apply cartesian-product args)
               (compose1 (call (wrapper-procedure procedure)) (mapM unitL))))

      ((if (wrapper? proc) call-wrapped call-primitive) proc args))

    (define (n:procedure? v)
      (or (procedure? v) (wrapper? v)))

    (define amb-apply
      (wrap
       (lambda (proc args)
         (bindM proc (lambda (p) (bindM args (lambda (a) (call-all p (mapM unitL a)))))))))))
