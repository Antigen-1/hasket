(module abstract "../base/main.rkt"
  (require (for-syntax racket/match "../base/main.rkt" "utilities.rkt"))
  (provide (all-defined-out) (for-syntax (all-defined-out)))

  (define-syntax (amb stx)
    (raise-syntax-error #f "Used out of amb-begin" stx))
  (define-syntax (ramb stx)
    (raise-syntax-error #f "Used out of amb-begin" stx))

  ;; Abstract syntax
  (begin-for-syntax
    (define-match-expander let-clause
      (syntax-rules ()
        ((_ name expr) (app syntax-e `(,(? (identifier=? #'let)) ,(? identifier? name) ,expr)))))
    (define-match-expander amb-clause
      (syntax-rules ()
        ((_ choices) (app syntax-e `(,(? (identifier=? #'amb)) ,@choices)))))
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
        ((_ proc args) (app syntax-e (or `(,(? (identifier=? #'#%app)) ,proc ,@args)
                                         `(,(? (lambda (v) (or (not (identifier? v))
                                                               (andmap (compose1 not (identifier=? v)) (list #'let #'begin #'quote #'#%app #'#%top #'if #'lambda #'n:amb #'ramb))))
                                               proc)
                                           ,@args))))))
    (define-match-expander quote-clause
      (syntax-rules ()
        ((_ datum) (app syntax-e (or `(,(? (identifier=? #'quote)) ,datum) (? (not . (lambda (v) (or (symbol? v) (pair? v)))) datum))))))
    (define-match-expander var-clause
      (syntax-rules ()
        ((_ var) (or (app syntax-e `(,(? (identifier=? #'#%top)) . ,(? identifier? var))) (? identifier? var)))))))
