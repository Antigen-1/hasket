(module main "../base/main.rkt"
  (require "syntax.rkt" "procedure.rkt")
  (provide (all-from-out "syntax.rkt")
           (rename-out (n:procedure? amb-procedure?)
                       (procedure? amb-primitive?))
           amb-apply))
