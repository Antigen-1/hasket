#lang racket/base
(require racket/match racket/list (for-syntax racket/base))
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
    ((_ first others ...)
     (with-syntax ((it (car (syntax-e (syntax-local-introduce #'(it stx)))))) ;; 只开放it
      #'(let ((next (passes others ...))) ;; 避免ambiguous binding
          (lambda (it)
            (define result first)
            (next result)))))
    ((_)
     #'values)))

(define ((make-pipeline-optimizer >>> >>>/steps Left Right catch) stx)
  (define-values (identifier=>>>?
                  identifier=>>>/steps?
                  identifier=Left?
                  identifier=Right?
                  identifier=catch?)
    (apply values (map make-identifier=? (list >>> >>>/steps Left Right catch))))

  (define optimize-catch-or-steps
    (passes
     ;; 递归进入复合步骤，将没有catcher或没有step的复合步骤inline入上级步骤列表
     (flatten (map (lambda (st) (match (syntax-e st)
                                  (`(,op ,sts ...)
                                   #:when (identifier=>>>/steps? op)
                                   (define opt (optimize-catch-or-steps sts))
                                   (return-if/else opt no-catcher? (datum->syntax st `(,op ,@opt))))
                                  (_ st)))
                   it))
     ;; 递归进入catcher
     (map (lambda (st) (match (syntax-e st) (`(,prefix ,body ...) #:when (identifier=catch? prefix) (datum->syntax st `(,prefix ,@(optimize-catch-or-steps body)))) (_ st))) it)

     ;; Right相当于values
     (filter-not identifier=Right? it)
     ;; Left之后的永远不会执行
     (take-until it identifier=Left?)
     ;; 末尾的catcher没有意义
     (dropf-right it catcher?)
     ))

  (define (step? st) (match (syntax-e st) (`(,prefix ,body ...) #:when (identifier=catch? prefix) #f) (_ #t)))
  (define (catcher? st) (not (step? st)))
  (define (no-catcher? sts)
    (null? (filter catcher? sts)))

  (match (syntax-e stx)
    (`(,op ,v ,sts ...)
     #:when (identifier=>>>? op)
     (datum->syntax stx (let/cc cc `(,op ,v ,@(return-if/else (optimize-catch-or-steps sts) (lambda (sts) (not (null? sts))) (cc v)))))) ;; 如果没有step，直接返回输入值
    (`(,op ,sts ...)
     #:when (identifier=>>>/steps? op)
     (datum->syntax stx (let/cc cc `(,op ,@(return-if/else (optimize-catch-or-steps sts) (lambda (sts) (not (null? sts))) (cc Right)))))) ;; 如果没有step，直接返回Right
    (_ (raise-syntax-error #f "Ill-formed expression" stx))))
