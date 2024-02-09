(module main "../base/main.rkt"
  (require "syntax.rkt" "procedure.rkt" "functions.rkt" "abstract.rkt")
  (provide (all-from-out "syntax.rkt")
           (rename-out (n:procedure? amb-procedure?)
                       (procedure? amb-primitive?))
           amb-apply amb-make-procedure/arbitrary-arity
           (all-from-out "functions.rkt")
           amb ramb))
