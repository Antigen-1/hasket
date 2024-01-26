#lang racket/base
(require scribble/base syntax/parse/define (for-syntax racket/base))
(provide todo-itemlist)

(begin-for-syntax
  (define-splicing-syntax-class item
    #:description "事物"
    (pattern (~seq #:working cont)
             #:with done? #'null
             #:with content #'cont)
    (pattern (~seq #:done cont)
             #:with done? #'#t
             #:with content #'cont)
    (pattern (~seq cont)
             #:with done? #'#f
             #:with content #'cont)))

(define-syntax-parse-rule (todo-itemlist i:item ...)
  (itemlist (item ((cond ((null? i.done?) (lambda (c) (list c ": working!")))
                         (i.done? (lambda (c) (list c ": done!")))
                         (else bold))
                   i.content))
            ...))
