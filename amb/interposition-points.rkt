(module ip "../base/main.rkt"
  (require (for-syntax "../base/main.rkt")
           "procedure.rkt"
           syntax/parse/define)
  (provide n:#%app n:lambda n:quote n:#%top n:if n:let amb)

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
  (define-syntax-parse-rule (n:#%top . v)
    (unitL v)))
