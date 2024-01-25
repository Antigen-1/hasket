(module syntax "../base/main.rkt"
  (require racket/stxparam syntax/parse/define racket/runtime-path "namespace.rkt"
           (for-syntax racket/match syntax/strip-context "../base/main.rkt")
           (for-meta 2 "../base/main.rkt"))
  (provide amb-begin (rename-out (n:amb amb)))

  (module interposition-points "../base/main.rkt"
    (require (for-syntax "../base/main.rkt")
             (submod racket/performance-hint begin-encourage-inline)
             racket/stxparam racket/list
             syntax/parse/define)
    (provide n:#%app n:lambda n:quote n:#%top n:if top amb
             amb-apply)

    ;; Inline instructions
    (begin-encourage-inline
      (struct wrapper (procedure))

      (define (call-all proc args)
        (define call (curry/n apply 2))
        (define (call-primitive procedure args)
          (bindM (apply cartesian-product args)
                 (compose1 unitL (call procedure))))

        (if (wrapper? proc)
            (call (wrapper-procedure proc) args)
            (call-primitive proc args)))
      (define amb-apply
        (wrapper
         (lambda (p args)
           (joinM (n:#%app (unitL call-all) p (mapM (mapM unitL) args))))))
      )

    (define-syntax-parse-rule (amb choice ...)
      (append choice ...))
    ;; 只有当执行primitive函数或遇到分支时才会真正进行组合
    ;; -------------------------------------------
    (define-syntax-parse-rule (n:#%app proc arg ...)
      (bindM proc (lambda (p) (call-all p (list arg ...)))))
    (define-syntax-parse-rule (n:if test then else)
      (bindM test (lambda (t) (if t then else))))
    ;; -------------------------------------------
    (define-syntax-parse-rule (n:quote datum)
      (unitL (quote datum)))
    (define-syntax-parse-rule (n:lambda (arg:id ...) body ...)
      (unitL (wrapper (lambda (arg ...) body ...))))
    ;; 用来实现#%top
    (define-syntax-parameter top (syntax-rules ()))
    (define-syntax-parse-rule (n:#%top . v:id)
      (unitL (top . v))))

  (require 'interposition-points)
  (define-runtime-module-path-index interposition-points '(submod "." interposition-points))

  (define-syntax (n:amb stx)
    (raise-syntax-error #f "Used out of amb-begin" stx))

  (begin-for-syntax
    (define identifer-symbol=?
      (lambda/curry/match
       ((id1 id2)
        (eq? (syntax->datum id1) (syntax->datum id2)))))

    ;; quote #%app lambda if #%top n:amb是用作hook
    ;; 所有变量的绑定和使用均去除了原有的context
    (define expand-statement-list
      (lambda/curry/match
       ((qut app lmd top amb nif `(,fst ,ost ...) available-variables)
        (define recursive-expand (expand-statement-list qut app lmd top amb nif))
        (match (syntax-e fst)
          (`(,let ,name ,expr)
           #:when (and (identifier? let) (free-identifier=? let #'let) (identifier? name))
           (list `(,#'letrec ((,(strip-context name) ,@(recursive-expand (list expr) (cons name available-variables))))
                             ,@(recursive-expand ost (cons name available-variables)))))
          (`(,lambda ,args ,bodies ...)
           #:do [(define unwrapped (syntax-e args))]
           #:when (and (identifier? lambda)
                       (free-identifier=? lambda #'lambda)
                       (list? unwrapped)
                       (andmap identifier? unwrapped))
           (cons `(,lmd ,(map strip-context unwrapped) ,@(recursive-expand bodies (append unwrapped available-variables)))
                 (recursive-expand ost available-variables)))
          (`(,begin ,sts ...)
           #:when (and (identifier? begin) (free-identifier=? begin #'begin))
           (cons `(,#'begin ,@(recursive-expand sts available-variables))
                 (recursive-expand ost available-variables)))
          (`(,if ,test ,then ,else)
           #:when (and (identifier? if) (free-identifier=? if #'if))
           (cons `(,nif ,@(recursive-expand (list test) available-variables)
                        ,@(recursive-expand (list then) available-variables)
                        ,@(recursive-expand (list else) available-variables))
                 (recursive-expand ost available-variables)))
          (`(,a ,choices ...)
           #:when (and (identifier? a) (free-identifier=? a #'n:amb))
           (cons `(,amb ,@(bindM choices (lambda (ch) (recursive-expand (list ch) available-variables))))
                 (recursive-expand ost available-variables)))
          (`(,quote ,datum)
           #:when (and (identifier? quote) (free-identifier=? quote #'quote))
           (cons `(,qut ,datum) (recursive-expand ost available-variables)))
          (`(,a . ,l)
           #:when (and (identifier? a) (free-identifier=? a #'#%app) (list? (syntax-e l)))
           (cons `(,#'n:#%app . ,l) (recursive-expand ost available-variables)))
          (`(,t . ,id)
           #:when (and (identifier? t) (free-identifier=? t #'#%top) (identifier? id))
           (cons `(,#'n:#%top . ,id) (recursive-expand ost available-variables)))
          (`(,proc ,args ...)
           (cons `(,app ,@(bindM (cons proc args) (lambda (t) (recursive-expand (list t) available-variables))))
                 (recursive-expand ost available-variables)))
          (id
           #:when (and (symbol? id) (findf (identifer-symbol=? fst) available-variables))
           (cons (strip-context fst) (recursive-expand ost available-variables)))
          (id
           #:when (symbol? id)
           (cons `(,top . ,(strip-context fst)) (recursive-expand ost available-variables)))
          (v #:when (and (not (list? v)) (not (symbol? v)))
             (cons `(,qut ,v) (recursive-expand ost available-variables)))
          (_ (raise-syntax-error #f "Illegal statement" fst))))
       ((_ _ _ _ _ _ `() _)
        null)))

    (define (n:expand-statement-list sts) (expand-statement-list #'n:quote #'n:#%app #'n:lambda #'n:#%top #'amb #'n:if sts null))

    (define-splicing-syntax-class extensions
      #:description "添加进命名空间的模块"
      (pattern (~seq #:extensions (mod ...)) #:with mods #'(list mod ...))
      (pattern (~seq) #:with mods #'null)))

  (define-namespace-anchor anchor)

  (define-syntax (amb-begin stx)
    (syntax-parse stx
      ((_ ext:extensions statement ...)
       #`(let ((namespace (make-hasket-base-namespace)))
           (parameterize ((current-namespace namespace))
             (namespace-attach-module (namespace-anchor->empty-namespace anchor) (module-path-index-resolve interposition-points))
             (namespace-require (list 'only (module-path-index-resolve interposition-points) 'amb-apply))
             (map namespace-require ext.mods))
           (syntax-parameterize ((top (syntax-rules () ((_ . v) (namespace-refer namespace 'v 'amb-begin)))))
             #,@(n:expand-statement-list (syntax->list #'(statement ...))))))))

  (module* test "../base/main.rkt"
    (require rackunit racket/runtime-path (submod "..")
             (for-syntax "../base/main.rkt"))

    (check-equal? (amb-begin (amb 1)) '(1))
    (check-equal? (amb-begin 1) '(1))
    (check-eq? (car (amb-begin +)) +)
    (check-equal? (amb-begin (let a 1) (list a)) '((1)))
    (check-equal? (amb-begin (let op (amb + - * /)) (op 1 2)) '(3 -1 2 1/2))
    (check-equal? (amb-begin ((lambda (n1 n2) (+ n1 n2)) 1 2)) '(3))
    (check-equal? (amb-begin (if (amb #f #f #t) 1 2)) '(2 2 1))
    (check-equal? (amb-begin (amb-apply (lambda (n1 n2 n3) (amb-apply + (list n1 n2 n3))) (list (amb 0 1) 2 3))) '(5 6))

    (module temp "../base/main.rkt"
      (provide a add1)
      (define a 1))
    (define-runtime-module-path-index temp '(submod "." temp))
    (check-equal? (amb-begin #:extensions ((module-path-index-resolve temp)) (add1 a)) '(2))
    ))
