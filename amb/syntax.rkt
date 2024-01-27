(module syntax "../base/main.rkt"
  (require racket/stxparam syntax/parse/define racket/runtime-path "namespace.rkt"
           (for-syntax racket/match syntax/strip-context "../base/main.rkt")
           (for-meta 2 "../base/main.rkt"))
  (provide amb-begin (rename-out (n:amb amb)))

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

  (module interposition-points "../base/main.rkt"
    (require (for-syntax "../base/main.rkt")
             (submod ".." procedure)
             racket/stxparam
             syntax/parse/define)
    (provide n:#%app n:lambda n:quote n:#%top n:if n:let top amb)

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
    (define-syntax-parse-rule (n:lambda (arg ...) body ...)
      (unitL (wrap (lambda (arg ...) body ...))))
    (define-syntax-parse-rule (n:let name expr body ...)
      (bindM expr ((lambda (name) body ...) . unitL)))
    ;; 用来实现#%top
    (define-syntax-parameter top (syntax-rules ()))
    (define-syntax-parse-rule (n:#%top . v)
      (unitL (top . v))))

  (require 'interposition-points)

  (module primitives "../base/main.rkt"
    (require (submod ".." procedure))
    (provide amb-apply
             (rename-out (procedure? primitive?)
                         (n:procedure? procedure?))))

  (require 'primitives)
  (define-runtime-module-path-index primitives '(submod "." primitives))

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
       ((lt qut app lmd top amb nif name `(,fst ,ost ...) available-variables)
        (define recursive-expand/name (expand-statement-list lt qut app lmd top amb nif))
        (define recursive-expand (recursive-expand/name #f))
        (define (maybe-wrap-name stx)
          ;; 支持递归使用的工具函数
          (if name
              #`(letrec ((#,name #,stx))
                  #,name)
              stx))
        (define (wrap-expr stx)
          ;; 打包为一个整体
          #`(let () #,@stx))
        (match (syntax-e fst)
          (`(,quote ,datum)
           #:when (and (identifier? quote) (free-identifier=? quote #'quote))
           (cons `(,qut ,datum) (recursive-expand ost available-variables)))
          (`(,t . ,id)
           #:when (and (identifier? t) (free-identifier=? t #'#%top) (identifier? id))
           (cons `(,#'n:#%top . ,id) (recursive-expand ost available-variables)))
          (id
           #:when (and (symbol? id) (findf (identifer-symbol=? fst) available-variables))
           (cons (strip-context fst) (recursive-expand ost available-variables)))
          (id
           #:when (symbol? id)
           (cons `(,top . ,(strip-context fst)) (recursive-expand ost available-variables)))
          (v #:when (and (not (list? v)) (not (symbol? v)))
             (cons `(,qut ,v) (recursive-expand ost available-variables)))
          ;; name总是由let form输入，amb form接收使用并传递给choices，以下的其他形式只会接收使用name
          ;; ---------------------------------------------------------------------
          (`(,let ,name ,expr)
           #:when (and (identifier? let) (free-identifier=? let #'let) (identifier? name))
           (list `(,lt
                   ,(strip-context name)
                   ,(wrap-expr (recursive-expand/name (strip-context name) (list expr) (cons name available-variables)))
                   ,@(recursive-expand ost (cons name available-variables)))))
          (`(,a ,choices ...)
           #:when (and (identifier? a) (free-identifier=? a #'n:amb))
           (cons `(,amb ,@(bindM choices (lambda (ch) (list (maybe-wrap-name (wrap-expr (recursive-expand/name name (list ch) available-variables)))))))
                 (recursive-expand ost available-variables)))
          (`(,lambda ,args ,bodies ...)
           #:do [(define unwrapped (syntax-e args))]
           #:when (and (identifier? lambda)
                       (free-identifier=? lambda #'lambda)
                       (list? unwrapped)
                       (andmap identifier? unwrapped))
           (cons (maybe-wrap-name `(,lmd ,(map strip-context unwrapped) ,(wrap-expr (recursive-expand bodies (append unwrapped available-variables)))))
                 (recursive-expand ost available-variables)))
          (`(,begin ,sts ...)
           #:when (and (identifier? begin) (free-identifier=? begin #'begin))
           (cons (maybe-wrap-name `(,#'begin ,(wrap-expr (recursive-expand sts available-variables))))
                 (recursive-expand ost available-variables)))
          (`(,if ,test ,then ,else)
           #:when (and (identifier? if) (free-identifier=? if #'if))
           (cons (maybe-wrap-name
                  `(,nif ,(wrap-expr (recursive-expand (list test) available-variables))
                         ,(wrap-expr (recursive-expand (list then) available-variables))
                         ,(wrap-expr (recursive-expand (list else) available-variables))))
                 (recursive-expand ost available-variables)))
          (`(,a ,proc ,args ...)
           #:when (and (identifier? a) (free-identifier=? a #'#%app))
           (cons (maybe-wrap-name `(,app ,@(bindM (cons proc args) (lambda (t) (recursive-expand (list t) available-variables)))))
                 (recursive-expand ost available-variables)))
          (`(,proc ,args ...)
           (cons (maybe-wrap-name `(,app ,@(bindM (cons proc args) (lambda (t) (recursive-expand (list t) available-variables)))))
                 (recursive-expand ost available-variables)))
          ;; ---------------------------------------------------------------------
          (_ (raise-syntax-error #f "Illegal statement" fst))))
       ((_ _ _ _ _ _ _ _ `() _)
        null)))

    (define (n:expand-statement-list sts) (expand-statement-list #'n:let #'n:quote #'n:#%app #'n:lambda #'n:#%top #'amb #'n:if #f sts null))

    (define-splicing-syntax-class extensions
      #:description "添加进命名空间的模块"
      (pattern (~seq #:extensions (mod ...)) #:with mods #'(list mod ...))
      (pattern (~seq) #:with mods #'null)))

  (define-namespace-anchor anchor)

  (define-syntax (amb-begin stx)
    (syntax-parse stx
      ((_ ext:extensions statement ...)
       #`(parameterize ((compile-enforce-module-constants #t))
           (let ((namespace (make-hasket-base-namespace)))
             (namespace-require! primitives (namespace-anchor->empty-namespace anchor) namespace)
             (parameterize ((current-namespace namespace))
                 (map namespace-require ext.mods))
             (syntax-parameterize ((top (syntax-rules () ((_ . v) (namespace-refer namespace 'v 'amb-begin)))))
               #,@(n:expand-statement-list (syntax->list #'(statement ...)))))))))

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
    (check-equal? (amb-begin (amb-apply + (list (amb 0 1) 2 3))) '(5 6))
    (check-equal? (amb-begin (primitive? +)) '(#t))
    (check-equal? (amb-begin (primitive? (lambda (a) a))) '(#f))
    (check-equal? (amb-begin (procedure? +)) '(#t))
    (check-equal? (amb-begin (procedure? (lambda (a) a))) '(#t))
    (check-equal? (amb-begin (let map (lambda (p l) (if (null? l) null (cons (p (car l)) (map p (cdr l)))))) (map add1 (list 1 2))) '((2 3)))
    (check-equal? (amb-begin (if (begin (let a (amb #f #t)) a) 1 2)) '(2 1))
    (check-equal? (amb-begin (let make (lambda () (amb 1 2 3)))
                             (let a1 (make))
                             (let result0 (list a1))
                             (let a2 (make))
                             (if (= a1 a2)
                                 (amb)
                                 (begin
                                   (let result1 (cons a2 result0))
                                   (reverse result1))))
                  '((1 2)
                    (1 3)
                    (2 1)
                    (2 3)
                    (3 1)
                    (3 2)))

    (module temp "../base/main.rkt"
      (provide a add1 false true)
      (define a 1)
      (define (false) #f)
      (define (true) #t))
    (define-runtime-module-path-index temp '(submod "." temp))
    (check-equal? (amb-begin #:extensions ((module-path-index-resolve temp)) (add1 a)) '(2))
    (check-equal? (amb-begin #:extensions ((module-path-index-resolve temp)) (if (false) 1 (if (true) (amb) 1))) '())
    ))
