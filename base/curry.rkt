#lang racket/base
(require racket/function racket/contract racket/match
         syntax/parse/define
         (for-syntax racket/base racket/list))
(provide lambda/curry/match curry/n)

;; 注意：整个模块都不支持关键字参数

(begin-for-syntax
  (define-splicing-syntax-class name
    #:description "函数名"
    (pattern (~seq ) #:with name #'temp)
    (pattern (~seq #:name nm:id) #:with name #'nm)))
(define-syntax-parser lambda/curry/match ;; match-lambda**不支持可变参数，因此这里不需要输入arity
  [(_ nm:name ((~datum !) contract) match-clause ...)
   #'(let () (define/contract nm.name contract (match-lambda** match-clause ...)) (curry nm.name))]
  [(_ nm:name match-clause:expr ...) ;; 尽管match的模式可能是关键字，最外层依旧必须是list
   #'(let () (define nm.name (match-lambda** match-clause ...)) (curry nm.name))])

(begin-for-syntax
  (define-splicing-syntax-class curried-procedure-and-its-arity
    #:description "函数及指定的元数"
    (pattern (~seq procedure arity:exact-nonnegative-integer)
             #:declare procedure (expr/c
                                  #'(and/c procedure?
                                           (lambda (p) (bitwise-bit-set? (procedure-arity-mask p) arity)))))))
(define-syntax-parser curry/n
  [(_ proc-arity:curried-procedure-and-its-arity)
   (with-syntax (((arg ...) (generate-temporaries (range (syntax->datum #'proc-arity.arity)))))
     #'(let ((temp (lambda (arg ...) (proc-arity.procedure arg ...))))
         (curry temp)))])

(module+ test
  (require rackunit)
  (check-eq? 'curried:a (object-name (lambda/curry/match #:name a (! (-> void?)) (() (void)))))
  (check-eq? 'curried:a (object-name (lambda/curry/match #:name a (() (void)))))
  (check-true ((lambda/curry/match ((b) #:when (not b) #t)) #f))
  (define n= (lambda/curry/match (! (-> integer? integer? boolean?)) ((i n) (= i n))))
  (check-true ((n= 1) 1))
  (define nn= (lambda/curry/match ((i n) (= i n))))
  (check-false ((nn= 1) 3))
  (define a (lambda/curry/match ((#:test) #t)))
  (check-true (a '#:test))
  (define n+ (curry/n + 2))
  (check-true (= ((n+ 1) 2) 3)))
