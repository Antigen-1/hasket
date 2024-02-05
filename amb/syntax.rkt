(module syntax "../base/main.rkt"
  (require syntax/parse/define "interposition-points.rkt"
           (for-syntax racket/match racket/list syntax/strip-context "../base/main.rkt")
           (for-meta 2 "../base/main.rkt"))
  (provide amb-begin (rename-out (n:amb amb)) ramb)

  (define-syntax (n:amb stx)
    (raise-syntax-error #f "Used out of amb-begin" stx))
  (define-syntax (ramb stx)
    (raise-syntax-error #f "Used out of amb-begin" stx))

  (begin-for-syntax
    ;; Utilities
    (define identifer-symbol=?
      (lambda/curry/match
       ((id1 id2)
        (eq? (syntax->datum id1) (syntax->datum id2)))))
    (define identifier=?
      (lambda/curry/match
       ((base v)
        (and (identifier? v) (free-identifier=? base v)))))
    (define self-evaluating?
      (lambda/curry/match
       ((avars v)
        (match v
          ((quote-clause _) #t)
          ((var-clause v) #:when (not (findf (identifer-symbol=? v) avars)) #t)
          (_ #f)))))
    (define (get-value v)
      (match v
        ((quote-clause v) `(,#'quote ,v))
        ((var-clause v) v)))
    (define (wrap-expr stx)
      ;; 打包为一个整体
      #`(let () #,@stx))
    (define ((make-maybe-wrap-name name) stx)
      ;; 支持递归使用的工具函数
      (if name
          #`(letrec ((#,name #,stx))
              #,name)
          stx))

    ;; Abstract syntax
    (define-match-expander let-clause
      (syntax-rules ()
        ((_ name expr) (app syntax-e `(,(? (identifier=? #'let)) ,(? identifier? name) ,expr)))))
    (define-match-expander amb-clause
      (syntax-rules ()
        ((_ choices) (app syntax-e `(,(? (identifier=? #'n:amb)) ,@choices)))))
    (define-match-expander ramb-clause
      (syntax-rules ()
        ((_ choices) (app syntax-e `(,(? (identifier=? #'ramb)) ,@choices)))))
    (define-match-expander lambda-clause
      (syntax-rules ()
        ((_ args bodies)
         (app syntax-e
              `(,(? (identifier=? #'lambda))
                ,(app syntax-e (? list? (? (lambda (l) (andmap identifier? l)) args)))
                ,@bodies)))))
    (define-match-expander begin-clause
      (syntax-rules ()
        ((_ sts) (app syntax-e `(,(? (identifier=? #'begin)) ,@sts)))))
    (define-match-expander if-clause
      (syntax-rules ()
        ((_ test then alt) (app syntax-e `(,(? (identifier=? #'if)) ,test ,then ,alt)))))
    (define-match-expander app-clause
      (syntax-rules ()
        ((_ proc args) (app syntax-e (or `(,(? (identifier=? #'#%app)) ,proc ,@args) `(,proc ,@args))))))
    (define-match-expander quote-clause
      (syntax-rules ()
        ((_ datum) (app syntax-e (or `(,(? (identifier=? #'quote)) ,datum) (? (not . (lambda (v) (or (symbol? v) (pair? v)))) datum))))))
    (define-match-expander var-clause
      (syntax-rules ()
        ((_ var) (or (app syntax-e `(,(? (identifier=? #'#%top)) . ,(? identifier? var))) (? identifier? var)))))

    ;; Optimizers
    (define (make-if nif test then alt avars expand)
      (cond ((andmap (self-evaluating? avars) (list test then alt))
             `(,#'#%app ,#'unitL ,(if (get-value test) (get-value then) (get-value alt))))
            ((self-evaluating? avars test)
             `(,#'if ,(get-value test) ,(wrap-expr (expand (list then) avars)) ,(wrap-expr (expand (list alt) avars))))
            (else `(,nif ,(wrap-expr (expand (list test) avars))
                         ,(wrap-expr (expand (list then) avars))
                         ,(wrap-expr (expand (list alt) avars))))))
    (define (make-let nlet name expr bodies avars expand/name)
      (cond ((self-evaluating? avars expr)
             `(,#'let ((,(strip-context name) (,#'#%app ,#'unitL ,(get-value expr))))
                      ,@(expand/name #f bodies (cons name avars))))
            (else `(,nlet ,(strip-context name) ,(wrap-expr (expand/name (strip-context name) (list expr) (cons name avars)))
                          ,@(expand/name #f bodies (cons name avars))))))
    (define (make-amb #:shuffle? shuffle?
                      amb choices
                      avars name
                      expand/name
                      )
      (define post (if shuffle? shuffle values))
      (cond ((andmap (self-evaluating? avars) choices)
             `(,#'#%app ,#'list ,@(post (map get-value choices))))
            (else `(,amb ,@(post (mapM (lambda (ch) ((make-maybe-wrap-name name) (wrap-expr (expand/name name (list ch) avars)))) choices))))))

    ;; 所有amb-begin内绑定的变量及其引用均去除了原有的context
    (define expand-statement-list
      (lambda/curry/match
       ((lt qut app lmd top amb nif name `(,fst ,ost ...) available-variables)
        (define recursive-expand/name (expand-statement-list lt qut app lmd top amb nif))
        (define recursive-expand (recursive-expand/name #f))
        (define maybe-wrap-name (make-maybe-wrap-name name))
        (match fst
          ((quote-clause datum)
           (cons `(,qut ,datum) (recursive-expand ost available-variables)))
          ((var-clause id)
           #:when (findf (identifer-symbol=? id) available-variables)
           (cons (strip-context id) (recursive-expand ost available-variables)))
          ((var-clause id)
           (cons `(,top . ,id) (recursive-expand ost available-variables)))
          ;; name总是由let form输入，amb form接收使用并传递给choices，以下的其他形式只会接收使用name
          ;; ---------------------------------------------------------------------
          ((let-clause name expr)
           (list (make-let lt name expr ost available-variables recursive-expand/name)))
          ((amb-clause choices)
           (cons (make-amb #:shuffle? #f amb choices available-variables name recursive-expand/name)
                 (recursive-expand ost available-variables)))
          ((ramb-clause choices)
           (cons (make-amb #:shuffle? #t amb choices available-variables name recursive-expand/name)
                 (recursive-expand ost available-variables)))
          ((lambda-clause unwrapped bodies)
           (cons (maybe-wrap-name `(,lmd ,(map strip-context unwrapped) ,(wrap-expr (recursive-expand bodies (append unwrapped available-variables)))))
                 (recursive-expand ost available-variables)))
          ((begin-clause sts)
           (cons (maybe-wrap-name `(,#'begin ,(wrap-expr (recursive-expand sts available-variables))))
                 (recursive-expand ost available-variables)))
          ((if-clause test then alt)
           (cons (maybe-wrap-name (make-if nif test then alt available-variables recursive-expand))
                 (recursive-expand ost available-variables)))
          ((app-clause proc args)
           (cons (maybe-wrap-name `(,app ,@(bindM (cons proc args) (lambda (t) (recursive-expand (list t) available-variables)))))
                 (recursive-expand ost available-variables)))
          ;; ---------------------------------------------------------------------
          (_ (raise-syntax-error #f "Illegal statement" fst))))
       ((_ _ _ _ _ _ _ _ `() _)
        null)))

    (define (n:expand-statement-list sts) (expand-statement-list #'n:let #'n:quote #'n:#%app #'n:lambda #'n:#%top #'amb #'n:if #f sts null))
    )

  (define-syntax (amb-begin stx)
    (syntax-parse stx
      ((_ statement ...)
       #`(let ()
           #,@(n:expand-statement-list (syntax->list #'(statement ...)))))))

  (module* test "../base/main.rkt"
    (require rackunit (submod "..") "procedure.rkt"
             (for-syntax "../base/main.rkt"))

    (check-equal? (amb-begin (amb 1)) '(1))
    (check-equal? (sort (amb-begin (ramb 1 2)) <) (amb-begin (amb 1 2)))
    (check-equal? (amb-begin 1) '(1))
    (check-eq? (car (amb-begin +)) +)
    (check-eq? (car (amb-begin (#%top . +))) +)
    (check-equal? (amb-begin (let a 1) (list a)) '((1)))
    (check-equal? (amb-begin (let op (amb + - * /)) (op 1 2)) '(3 -1 2 1/2))
    (check-equal? (amb-begin ((lambda (n1 n2) (+ n1 n2)) 1 2)) '(3))
    (check-equal? (amb-begin (if (amb #f #f #t) 1 2)) '(2 2 1))
    (check-equal? (amb-begin (amb-apply (lambda (n1 n2 n3) (amb-apply + (list n1 n2 n3))) (list (amb 0 1) 2 3))) '(5 6))
    (check-equal? (amb-begin (amb-apply + (list (amb 0 1) 2 3))) '(5 6))
    (check-equal? (amb-begin (procedure? +)) '(#t))
    (check-equal? (amb-begin (procedure? (lambda (a) a))) '(#f))
    (check-equal? (amb-begin (n:procedure? +)) '(#t))
    (check-equal? (amb-begin (n:procedure? (lambda (a) a))) '(#t))
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
    (let ((a 1))
      (check-equal? (amb-begin (add1 a)) '(2)))
    ))
