(module ip "../base/main.rkt"
  (require (for-syntax "../base/main.rkt")
           "procedure.rkt"
           syntax/parse/define)
  (provide n:#%app n:lambda n:quote n:#%top n:if n:let o:lambda o:#%app amb)

  (define-syntax-parse-rule (amb choice ...)
    (append choice ...))

  ;; n:#%app : L (a ... -> L a) (L a) ... -> L a
  (define-syntax-parse-rule (n:#%app proc arg ...)
    (apply-amb-procedure proc (list arg ...)))
  ;; n:if : (L a) (L a) (L a) -> L a
  (define-syntax-parse-rule (n:if test then else)
    (bindM test (lambda (t) (if t then else))))

  (define-syntax-parse-rule (n:quote datum)
    (unitL (quote datum)))
  (define-syntax-parse-rule (n:lambda (arg ...) body ...)
    (unitL (wrap (lambda (arg ...) body ...))))
  (define-syntax-parse-rule (n:#%top . v)
    (unitL v))

  ;; Optimization
  (define-syntax-parse-rule (o:lambda (arg ...) body ...)
    (opt-wrap (lambda (arg ...) body ...)))
  (define-syntax-parse-rule (o:#%app proc arg ...)
    (call/opt proc (list arg ...)))

  ;; Sugar
  (define-syntax-parse-rule (n:let name expr body ...)
    (n:#%app (n:lambda (name) body ...) expr)))
