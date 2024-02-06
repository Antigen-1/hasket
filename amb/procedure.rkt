(module procedure "../base/main.rkt"
  (require (submod racket/performance-hint begin-encourage-inline) racket/list)
  (provide wrap opt-wrap call/opt apply-amb-procedure amb-apply n:procedure?)
  ;; Inline instructions
  (begin-encourage-inline
    (struct wrapper (procedure) #:constructor-name wrap)
    (struct opt-wrapper (procedure) #:constructor-name opt-wrap)

    (define (apply-amb-procedure proc args)

      (define (call-all proc args)
        (define call (curry/n apply 2))
        (define (call-primitive procedure args)
          (bindM (apply cartesian-product args)
                 (compose1 unitL (call procedure))))
        (define (call-wrapped procedure args)
          (bindM (apply cartesian-product args)
                 (compose1 (call (wrapper-procedure procedure)) (mapM unitL))))
        (define (call-opt-wrapped procedure args)
          (call-primitive (opt-wrapper-procedure procedure) args))

        ((cond ((wrapper? proc) call-wrapped)
               ((opt-wrapper? proc) call-opt-wrapped)
               (else call-primitive))
         proc args))

      (bindM proc (lambda (p) (call-all p args))))

    ;; Optimization
    (define (call/opt proc args)
      (cond ((wrapper? proc) (apply (wrapper-procedure proc) (mapM unitL args)))
            (else (apply (if (opt-wrapper? proc) (opt-wrapper-procedure proc) proc) args))))

    (define (n:procedure? v)
      (or (procedure? v) (wrapper? v) (opt-wrapper? v)))

    ;; Used in amb-begin
    (define amb-apply
      (wrap
       (lambda (proc args)
         (bindM args (lambda (as) (apply-amb-procedure proc (mapM unitL as)))))))
    ))
