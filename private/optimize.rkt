#lang racket/base
(require racket/match racket/list "pipeline.rkt" "monad.rkt" (for-syntax racket/base))
(provide make-pipeline-optimizer)

;; Utilities
(define (take-until l p)
  (if (null? l)
      l
      (if (p (car l)) (list (car l)) (cons (car l) (take-until (cdr l) p))))) ;; 最后检查的项仍然保留在结果中
(define ((make-identifier=? id) id1)
  (and (identifier? id1) (free-identifier=? id id1)))
(define-syntax-rule (return-if/else value pred else-body ...)
  (let ((result value))
    (cond ((pred result) result)
          (else else-body ...))))

;; Passes
(define-syntax (passes stx)
  (syntax-case stx ()
    ((_ step ...)
     (with-syntax ((it (car (syntax-e (syntax-local-introduce #'(it stx))))))
       #'(lambda (v) (>>> v (lambda (it) (unitP step)) ...))))))

(define-syntax >>>/steps/init #f) ;; A hook, not exported
(define ((make-pipeline-optimizer >>> >>>/steps o:>>> o:>>>/steps reset Left Right catch app lmd) stx)
  (define-values (identifier=>>>?
                  identifier=>>>/steps?
                  identifier=>>>/steps/init?
                  identifier=Left?
                  identifier=Right?
                  identifier=catch?)
    (apply values (map make-identifier=? (list >>> >>>/steps #'>>>/steps/init Left Right catch))))

  (define (step? st) (match (syntax-e st) (`(,prefix ,body ...) #:when (identifier=catch? prefix) #f) (_ #t)))
  (define (catcher? st) (not (step? st)))
  (define (has-catcher? sts)
    (not (null? (filter catcher? sts))))

  (define (Right-or-catcher? v)
    (or (identifier=Right? v)
        (catcher? v)))

  (define optimize-catch-or-steps
    (passes
     ;; 递归进入复合步骤，将没有catcher或没有step的复合步骤inline入上级步骤列表
     (flatten (map (lambda (st) (match (syntax-e st)
                                  (`(,op ,sts ...)
                                   #:when (identifier=>>>/steps? op)
                                   (define opt ((make-pipeline-optimizer >>> >>>/steps o:>>> o:>>>/steps reset Left Right catch app lmd) st))
                                   (define (get-step-list steps)
                                     (cdr (syntax->list steps)))
                                   (return-if/else opt
                                                   (lambda (v)
                                                     (or
                                                      ;; Right直接inline
                                                      ;; 有catcher不能inline
                                                      (identifier=Right? v)
                                                      (has-catcher? (get-step-list v))))
                                                   (get-step-list opt)))
                                  (_ st)))
                   it))
     ;; 递归进入catcher
     (map (lambda (st) (match (syntax-e st) (`(,prefix ,body ...) #:when (identifier=catch? prefix) (datum->syntax st `(,prefix ,@(optimize-catch-or-steps body)))) (_ st))) it)

     ;; Left之后的永远不会执行
     (take-until it identifier=Left?)
     ;; 末尾的catcher和Right都只能贡献position，而Right明显代码量更少
     (let-values (((former latter) (splitf-at-right it Right-or-catcher?))) (append former (map (lambda (_) Right) latter)))
     ))

  (define optimize-top-level-catch-or-steps
    (passes
     ;; 只有一个(top-level) step sublist，直接（递归）inline
     (let loop ((it it))
       (match it
         (`(,stx)
          (match (syntax-e stx)
            (`(,op ,sts ...)
             #:when (or (identifier=>>>/steps/init? op) (identifier=>>>/steps? op))
             (loop sts))
            (_ it)))
         (_ it)))
     ;; 常规sts优化
     (optimize-catch-or-steps it)
     ;; 递归进入top-level step list
     ;; >>>/steps/init不允许inline，除非step list为空
     ;; 保留下来的>>>/steps/init被替换为o:>>>/steps
     ;; 总之，top-level step list总是被原位保留的（即使是被inline），而且需要额外的处理，因此区别于一般的step list，单独放在这里
     (filter-map (lambda (st) (match (syntax-e st)
                                (`(,op ,sts ...)
                                 #:when (identifier=>>>/steps/init? op)
                                 (define nsts (optimize-top-level-catch-or-steps sts))
                                 (if (null? nsts)
                                     #f
                                     ;; A catcher is installed
                                     ;; Use reset to start a top-level step list
                                     (datum->syntax st `(,o:>>>/steps (,catch) (,lmd (v) (,app ,reset (,app (,o:>>>/steps ,@nsts) v)))))))
                                (_ st)))
                 it)
     ;; 末尾的catcher和Right无意义，但这里的所有catcher都已替换为了Right
     (dropf-right it identifier=Right?)
     ))

  (match (syntax-e stx)
    (`(,op ,v ,sts ...)
     #:when (identifier=>>>? op)
     ;; Top-level step list
     ;; 如果没有step，直接返回输入值
     (define-values (nv nsts)
       ;; 原始top-level step list使用>>>/steps/init封装
       (let loop ((v v) (sts (list (datum->syntax stx `(,#'>>>/steps/init ,@sts)))))
         (match (syntax-e v)
           ;; 递归进入value
           (`(,op ,nv ,extra-sts ...)
            #:when (identifier=>>>? op)
            ;; 新加入的top-level step list使用>>>/steps/init封装
            (define nsts (cons (datum->syntax v `(,#'>>>/steps/init ,@extra-sts)) sts))
            (loop nv nsts))
           (_ (values v sts)))))
     (datum->syntax stx (let/cc cc `(,o:>>> ,nv ,@(return-if/else (optimize-top-level-catch-or-steps nsts) (lambda (sts) (not (null? sts))) (cc nv))))))
    (`(,op ,sts ...)
     #:when (identifier=>>>/steps? op)
     ;; 如果没有step，直接返回Right
     (datum->syntax stx (let/cc cc `(,o:>>>/steps ,@(return-if/else (optimize-catch-or-steps sts) (lambda (sts) (not (null? sts))) (cc Right))))))
    (_ (raise-syntax-error #f "Ill-formed expression" stx))))
