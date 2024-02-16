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
                 (call (wrapper-procedure procedure))))
        (define (call-opt-wrapped procedure args)
          (call-primitive (opt-wrapper-procedure procedure) args))

        ((cond ((wrapper? proc) call-wrapped)
               ((opt-wrapper? proc) call-opt-wrapped)
               (else call-primitive))
         proc args))

      (bindM proc (lambda (p) (call-all p args))))

    ;; Optimization
    (define (call/opt3 proc args)
      ;; proc和args都可以优化
      ;; (-> (or/c wrapper? opt-wrapper? procedure?) (listof any/c) (list/c any/c))
      (cond ((wrapper? proc) (apply (wrapper-procedure proc) args))
            (else (unitL (call/opt2 proc args)))))
    (define (call/opt2 proc args)
      ;; proc和args都可以优化
      ;; (-> (or/c opt-wrapper? procedure?) (listof any/c) any)
      (apply (cond ((opt-wrapper? proc) (opt-wrapper-procedure proc))
                   (else proc))
             args))
    (define (call/opt1 proc args)
      ;; proc不能优化，args可以优化
      ;; (-> (listof (or/c wrapper? opt-wrapper? procedure?)) (listof any/c) (list/c any/c))
      (bindM
       proc
       (lambda (p)
         (cond ((wrapper? p) (apply (wrapper-procedure p) args))
               (else (unitL (call/opt2 p args)))))))

    (define (n:procedure? v)
      (or (procedure? v) (wrapper? v) (opt-wrapper? v)))

    ;; Used in amb-begin
    (define amb-apply (wrap call/opt3))
    (define (amb-make-procedure/arbitrary-arity proc)
      (wrap (lambda args (call/opt3 proc (unitL args)))))
    (define Yv (let ((make (lambda (m k x) (call/opt3 k (list (wrap (lambda (y) (m m k y))) x)))))
                 (lambda (k) (wrap (lambda (x) (make make k x))))))
    ))
