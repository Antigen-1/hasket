(module ip "../base/main.rkt"
  (require (for-syntax "../base/main.rkt")
           "procedure.rkt"
           syntax/parse/define)
  (provide n:#%app n:lambda n:quote n:#%top n:if n:let n:begin o:lambda o2:#%app o1:#%app amb)

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
    (unitL (wrap (lambda (arg ...) (let ((arg (unitL arg)) ...) body ...)))))
  (define-syntax-parse-rule (n:#%top . v)
    (unitL v))

  ;; Optimization
  (define-syntax-parse-rule (o:lambda (arg ...) body ...)
    (opt-wrap (lambda (arg ...) body ...)))
  (define-syntax-parse-rule (o2:#%app proc arg ...)
    (call/opt2 proc (list arg ...)))
  (define-syntax-parse-rule (o1:#%app proc arg ...)
    (call/opt1 proc (list arg ...)))

  ;; Sugar
  (define-syntax-parse-rule (n:let name expr body ...)
    (n:#%app (n:lambda (name) body ...) expr))
  (define-syntax-parser n:begin
    ((_ st1 st2 ost ...)
     #'(n:#%app (n:lambda (_) (n:begin st2 ost ...)) st1))
    ((_ st1) #'st1)))
