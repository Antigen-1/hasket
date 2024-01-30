(module syntax "../base/main.rkt"
  (require syntax/parse/define "interposition-points.rkt"
           (for-syntax racket/match syntax/strip-context "../base/main.rkt")
           (for-meta 2 "../base/main.rkt"))
  (provide amb-begin (rename-out (n:amb amb)))

  (define-syntax (n:amb stx)
    (raise-syntax-error #f "Used out of amb-begin" stx))

  (begin-for-syntax
    (define identifer-symbol=?
      (lambda/curry/match
       ((id1 id2)
        (eq? (syntax->datum id1) (syntax->datum id2)))))
    (define identifier=?
      (lambda/curry/match
       ((base v)
        (and (identifier? v) (free-identifier=? base v)))))

    ;; abstract syntax
    (define-match-expander let-clause
      (syntax-rules ()
        ((_ id1 id2) `(,(? (identifier=? #'let)) ,(? identifier? id1) ,id2))))
    (define-match-expander amb-clause
      (syntax-rules ()
        ((_ id) `(,(? (identifier=? #'n:amb)) ,@id))))
    (define-match-expander lambda-clause
      (syntax-rules ()
        ((_ id1 id2) `(,(? (identifier=? #'lambda))
                       ,(app syntax-e (? list? (? (lambda (l) (andmap identifier? l)) id1)))
                       ,@id2))))
    (define-match-expander begin-clause
      (syntax-rules ()
        ((_ id) `(,(? (identifier=? #'begin)) ,@id))))
    (define-match-expander if-clause
      (syntax-rules ()
        ((_ id1 id2 id3) `(,(? (identifier=? #'if)) ,id1 ,id2 ,id3))))
    (define-match-expander app-clause
      (syntax-rules ()
        ((_ id1 id2) (or `(,(? (identifier=? #'#%app)) ,id1 ,@id2) `(,id1 ,@id2)))))
    (define-match-expander quote-clause
      (syntax-rules ()
        ((_ id) (or `(,(? (identifier=? #'quote)) ,id) (? (not . (lambda (v) (or (symbol? v) (list? v)))) id)))))
    (define-match-expander top-clause
      (syntax-rules ()
        ((_ id) (or `(,(? (identifier=? #'#%top)) ,(? identifier? id)) (? symbol? (app (lambda (s) (datum->syntax #f s)) id))))))

    ;; 所有amb-begin内绑定的变量及其引用均去除了原有的context
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
          ((quote-clause datum)
           (cons `(,qut ,datum) (recursive-expand ost available-variables)))
          ((top-clause id)
           #:when (findf (identifer-symbol=? id) available-variables)
           (cons (strip-context fst) (recursive-expand ost available-variables)))
          ((top-clause id)
           (cons `(,top . ,fst) (recursive-expand ost available-variables)))
          ;; name总是由let form输入，amb form接收使用并传递给choices，以下的其他形式只会接收使用name
          ;; ---------------------------------------------------------------------
          ((let-clause name expr)
           (list `(,lt
                   ,(strip-context name)
                   ,(wrap-expr (recursive-expand/name (strip-context name) (list expr) (cons name available-variables)))
                   ,@(recursive-expand ost (cons name available-variables)))))
          ((amb-clause choices)
           (cons `(,amb ,@(bindM choices (lambda (ch) (list (maybe-wrap-name (wrap-expr (recursive-expand/name name (list ch) available-variables)))))))
                 (recursive-expand ost available-variables)))
          ((lambda-clause unwrapped bodies)
           (cons (maybe-wrap-name `(,lmd ,(map strip-context unwrapped) ,(wrap-expr (recursive-expand bodies (append unwrapped available-variables)))))
                 (recursive-expand ost available-variables)))
          ((begin-clause sts)
           (cons (maybe-wrap-name `(,#'begin ,(wrap-expr (recursive-expand sts available-variables))))
                 (recursive-expand ost available-variables)))
          ((if-clause test then else)
           (cons (maybe-wrap-name
                  `(,nif ,(wrap-expr (recursive-expand (list test) available-variables))
                         ,(wrap-expr (recursive-expand (list then) available-variables))
                         ,(wrap-expr (recursive-expand (list else) available-variables))))
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
    (check-equal? (amb-begin 1) '(1))
    (check-eq? (car (amb-begin +)) +)
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
