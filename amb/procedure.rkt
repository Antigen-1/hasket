(module procedure "../base/main.rkt"
  (require (submod racket/performance-hint begin-encourage-inline) racket/list)
  (provide wrap opt-wrap call/opt2 call/opt1 apply-amb-procedure amb-apply amb-make-procedure/arbitrary-arity Yv n:procedure?)
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
    (define (call/opt2 proc args)
      ;; proc和args都可以优化
      ;; 此时可以保证proc为opt-wrapper?或procedure?
      (apply (if (opt-wrapper? proc) (opt-wrapper-procedure proc) proc) args))
    (define (call/opt1 proc args)
      ;; proc不能优化，args可以优化
      ;; 此时proc可为wrapper?、opt-wrapper?或procedure?组成的列表
      (bindM
       proc
       (lambda (p)
         (cond ((wrapper? p) (apply (wrapper-procedure p) (mapM unitL args)))
               (else (unitL (apply (if (opt-wrapper? p) (opt-wrapper-procedure p) p) args)))))))

    (define (n:procedure? v)
      (or (procedure? v) (wrapper? v) (opt-wrapper? v)))

    ;; Used in amb-begin
    (define amb-apply
      (wrap
       (lambda (proc args)
         (apply call/opt1 proc args))))
    (define (amb-make-procedure/arbitrary-arity proc)
      (wrap (lambda args (call/opt1 (unitL proc) (unitL (append* args))))))
    (define Yv (let ((make (lambda (m k x) (call/opt1 k (cons (wrap (lambda (y) (m m k y))) x)))))
                 (lambda (k) (wrap (lambda (x) (make make (unitL k) x))))))
    ))
