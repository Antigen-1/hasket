(module syntax "../base/main.rkt"
  (require syntax/parse/define
           "interposition-points.rkt" (except-in "abstract.rkt" amb ramb) (only-in "procedure.rkt" n:procedure?)
           (for-syntax racket/match racket/list syntax/strip-context "../base/main.rkt" "utilities.rkt")
           (for-meta 2 "../base/main.rkt"))
  (provide amb-begin)

  (begin-for-syntax
    (define self-evaluating?
      (lambda/curry/match
       ((avars v)
        (match v
          ((quote-clause _) #t)
          ((var-clause v) #:when (not (findf (identifier-symbol=? v) avars)) #t)
          (_ #f)))))
    (define (get-value v)
      (match v
        ((quote-clause v) `(,#'quote ,v))
        ((var-clause v) v)))

    ;; Optimizers
    (define (make-amb #:shuffle? shuffle?
                      amb choices
                      avars name
                      expand/name
                      )
      (define post (if shuffle? shuffle values))
      (cond ((andmap (self-evaluating? avars) choices)
             `(,#'#%app ,#'list ,@(post (map get-value choices))))
            (else `(,amb ,@(post (mapM (lambda (ch) ((make-maybe-wrap-name name) (wrap-expr (expand/name name (list ch) avars)))) choices))))))
    (define (make-if nif test then alt avars expand)
      (define nthen (wrap-expr (expand (list then) avars)))
      (define nalt (wrap-expr (expand (list alt) avars)))
      (cond ((inline? avars (list test))
             `(,#'if ,(wrap-expr (inline avars (list test))) ,nthen ,nalt))
            (else `(,nif ,(wrap-expr (expand (list test) avars)) ,nthen ,nalt))))
    (define inline?
      (lambda/curry/match
       ((avars sts)
        (define inlined (syntax->list #'(;; Lists and Pairs
                                         pair? null? cons car cdr null list? list list* build-list length
                                         list-ref list-tail append reverse map andmap ormap for-each foldl foldr
                                         filter remove remq remv remw remove* remq* remv* remw* sort member memw
                                         memv memq memf findf assoc assw assv assq assf caar cadr cdar cddr caaar
                                         caadr cadar caddr cdaar cdadr cddar cdddr caaaar caaadr caadar caaddr
                                         cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr cddaar cddadr
                                         cdddar cddddr
                                         ;; Numbers
                                         number? complex? real? rational? integer? exact-integer?
                                         exact-nonnegative-integer? exact-positive-integer? inexact-real? fixnum?
                                         flonum? double-flonum? single-flonum? single-flonum-available?
                                         zero? positive? negative? even? odd? exact? inexact? inexact->exact
                                         exact->inexact real->single-flonum real->double-flonum + - * / quotient
                                         remainder #; quotient/remainder modulo add1 sub1 abs max min gcd lcm
                                         round floor ceiling truncate numerator denominator rationalize = < <= >
                                         >= sqrt integer-sqrt #; integer-sqrt/remainder expt exp log sin cos tan
                                         asin acos atan make-rectangular make-polar real-part imag-part magnitude
                                         angle bitwise-ior bitwise-and bitwise-xor bitwise-not bitwise-bit-set?
                                         bitwise-bit-field arithmetic-shift integer-length random random-seed
                                         make-pseudo-random-generator pseudo-random-generator?
                                         current-pseudo-random-generator pseudo-random-generator->vector
                                         vector->pseudo-random-generator vector->pseudo-random-generator!
                                         pseudo-random-generator-vector?
                                         ;; Equality
                                         equal? equal-always? eqv? eq? equal?/recur equal-always?/recur
                                         ;; Hasket
                                         bindM joinM mapM Right Left unitL unitS n:procedure?
                                         )))
        (define (inlined-statement? st)
          (match st
            (v #:when (self-evaluating? avars v) #t)
            ((let-clause _ expr) (inlined-statement? expr))
            ((amb-clause _) #f)
            ((ramb-clause _) #f)
            ((lambda-clause _ bodies) (andmap inlined-statement? bodies))
            ((begin-clause sts) (andmap inlined-statement? sts))
            ((if-clause test then alt) (andmap inlined-statement? (list test then alt)))
            ((app-clause proc args)
             (and (match proc
                    ((var-clause id) #:when (self-evaluating? avars id) (findf (identifier=? id) inlined))
                    (expr (inlined-statement? expr)))
                  (andmap inlined-statement? args)))
            (_ #f)))
        (andmap inlined-statement? sts))))
    (define (inline avars sts)
      (match sts
        (`(,fst ,@ost)
         (match fst
           (v #:when (self-evaluating? avars v)
              (cons (get-value v) (inline avars ost)))
           ((var-clause v) (cons (strip-context v) (inline avars ost)))
           ((let-clause name expr)
            (list `(,#'letrec ((,(strip-context name) ,(wrap-expr (inline (cons name avars) (list expr)))))
                              ,@(inline (cons name avars) ost))))
           ((lambda-clause args bodies)
            (cons `(,#'o:lambda (,@(map strip-context args)) ,@(inline (append args avars) bodies)) (inline avars ost)))
           ((begin-clause sts)
            (cons `(,#'begin ,@(inline avars sts)) (inline avars ost)))
           ((if-clause test then alt)
            (cons `(,#'if ,(wrap-expr (inline avars (list test)))
                          ,(wrap-expr (inline avars (list then)))
                          ,(wrap-expr (inline avars (list alt))))
                  (inline avars ost)))
           ((app-clause proc args)
            (cons `(,#'o:#%app ,@(mapM (lambda (t) (wrap-expr (inline avars (list t)))) (cons proc args)))
                  (inline avars ost)))
           (_ (raise-syntax-error #f "A statement that cannot be inlined" fst))))
        (`() null)))

    ;; 所有amb-begin内绑定的变量及其引用均去除了原有的context
    (define expand-statement-list
      (lambda/curry/match
       ((lt qut app lmd top amb nif name (and all `(,fst ,ost ...)) available-variables)
        (define recursive-expand/name (expand-statement-list lt qut app lmd top amb nif))
        (define recursive-expand (recursive-expand/name #f))
        (define maybe-wrap-name (make-maybe-wrap-name name))
        (if (inline? available-variables all)
            (list (wrap-unitL (wrap-expr (inline available-variables all))))
            (match fst
              ((quote-clause datum)
               (cons `(,qut ,datum) (recursive-expand ost available-variables)))
              ((var-clause id)
               #:when (not (self-evaluating? available-variables id))
               (cons (strip-context id) (recursive-expand ost available-variables)))
              ((var-clause id)
               (cons `(,top . ,id) (recursive-expand ost available-variables)))
              ;; name总是由let form输入，amb form接收使用并传递给choices，以下的其他形式只会接收使用name
              ;; ---------------------------------------------------------------------
              ((let-clause name expr)
               (list `(,lt ,(strip-context name) ,(wrap-expr (recursive-expand/name (strip-context name) (list expr) (cons name available-variables)))
                           ,@(recursive-expand ost (cons name available-variables)))))
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
              (_ (raise-syntax-error #f "Illegal statement" fst)))))
       ((_ _ _ _ _ _ _ _ `() _) null)))

    (define (n:expand-statement-list sts) (expand-statement-list #'n:let #'n:quote #'n:#%app #'n:lambda #'n:#%top #'amb #'n:if #f sts null))
    )

  (define-syntax (amb-begin stx)
    (syntax-parse stx
      ((_ statement ...)
       #`(let ()
           #,@(n:expand-statement-list (syntax->list #'(statement ...)))))))

  (module* test "../base/main.rkt"
    (require rackunit (submod "..") "procedure.rkt" (only-in "abstract.rkt" amb ramb)
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
    (check-equal? (amb-begin ((lambda () (amb 1 2)))) '(1 2))
    (check-equal? (amb-begin (amb-apply (lambda (n1 n2 n3) (amb-apply + (list n1 n2 n3))) (list (amb 0 1) 2 3))) '(5 6))
    (check-equal? (amb-begin (amb-apply + (list (amb 0 1) 2 3))) '(5 6))
    (check-equal? (amb-begin (procedure? +)) '(#t))
    (check-equal? (amb-begin (procedure? (lambda (a) a))) '(#f))
    (check-equal? (amb-begin (n:procedure? +)) '(#t))
    (check-equal? (amb-begin (n:procedure? (lambda (a) a))) '(#t))
    (check-equal? (amb-begin (let map (lambda (p l) (if (null? l) null (cons (p (car l)) (map p (cdr l)))))) (map add1 (list 1 2))) '((2 3)))
    (check-equal? (amb-begin (if (begin (let a (amb #f #t)) a) 1 2)) '(2 1))
    (check-equal? (amb-begin (if #t 1 (amb))) '(1))
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
