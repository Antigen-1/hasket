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

(define ((make-pipeline-optimizer >>> >>>/steps o:>>> o:>>>/steps Left Right catch) stx)
  (define-values (identifier=>>>?
                  identifier=>>>/steps?
                  identifier=Left?
                  identifier=Right?
                  identifier=catch?)
    (apply values (map make-identifier=? (list >>> >>>/steps Left Right catch))))

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
                                   (define opt ((make-pipeline-optimizer >>> >>>/steps o:>>> o:>>>/steps Left Right catch) st))
                                   (return-if/else opt
                                                   (lambda (v) (or (identifier=Right? v) (has-catcher? (cdr (syntax->list v)))))
                                                   (cdr (syntax->list opt))))
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
     ;; 常规sts优化
     (optimize-catch-or-steps it)
     ;; 末尾的catcher和Right无意义，但这里的所有catcher都已替换为了Right
     (dropf-right it identifier=Right?)
     ))

  (match (syntax-e stx)
    (`(,op ,v ,sts ...)
     #:when (identifier=>>>? op)
     ;; Top-level step list
     ;; 如果没有step，直接返回输入值
     (define-values (nv nsts)
       (let loop ((v v) (sts sts))
         (match (syntax-e v)
           ;; 递归进入value
           (`(,op ,nv ,extra-sts ...)
            #:when (identifier=>>>? op)
            (define nsts (cons (datum->syntax v `(,>>>/steps ,@extra-sts)) sts))
            (loop nv nsts))
           (_ (values v sts)))))
     (datum->syntax stx (let/cc cc `(,o:>>> ,nv ,@(return-if/else (optimize-top-level-catch-or-steps nsts) (lambda (sts) (not (null? sts))) (cc nv))))))
    (`(,op ,sts ...)
     #:when (identifier=>>>/steps? op)
     ;; 如果没有step，直接返回Right
     (datum->syntax stx (let/cc cc `(,o:>>>/steps ,@(return-if/else (optimize-catch-or-steps sts) (lambda (sts) (not (null? sts))) (cc Right))))))
    (_ (raise-syntax-error #f "Ill-formed expression" stx))))
