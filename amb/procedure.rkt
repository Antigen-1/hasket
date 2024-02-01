(module procedure "../base/main.rkt"
  (require (submod racket/performance-hint begin-encourage-inline) racket/list)
  (provide wrap apply-amb-procedure amb-apply n:procedure?)
  ;; Inline instructions
  (begin-encourage-inline
    (struct wrapper (procedure) #:constructor-name wrap)

    (define (apply-amb-procedure proc args)

      (define (call-all proc args)
        (define call (curry/n apply 2))
        (define (call-primitive procedure args)
          (bindM (apply cartesian-product args)
                 (compose1 unitL (call procedure))))
        (define (call-wrapped procedure args)
          (bindM (apply cartesian-product args)
                 (compose1 (call (wrapper-procedure procedure)) (mapM unitL))))

        ((if (wrapper? proc) call-wrapped call-primitive) proc args))

      (bindM proc (lambda (p) (call-all p args))))

    (define (n:procedure? v)
      (or (procedure? v) (wrapper? v)))

    ;; Used in amb-begin
    (define amb-apply
      (wrap
       (lambda (proc args)
         (bindM args (lambda (as) (apply-amb-procedure proc (mapM unitL as)))))))
    ))
