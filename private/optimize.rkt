#lang racket/base
(require racket/match racket/list)
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

(define ((make-pipeline-optimizer >>> >>>/steps Left Right catch) stx)
  (define-values (identifier=>>>?
                  identifier=>>>/steps?
                  identifier=Left?
                  identifier=Right?
                  identifier=catch?)
    (apply values (map make-identifier=? (list >>> >>>/steps Left Right catch))))

  (define (optimize-catch-or-steps sts)
    (define appended (flatten (map (lambda (st) (match (syntax-e st)
                                                  (`(,op ,sts ...)
                                                   #:when (identifier=>>>/steps? op)
                                                   (define opt (optimize-catch-or-steps sts))
                                                   (return-if/else opt no-catcher? (datum->syntax st `(,op ,@opt))))
                                                  (_ st)))
                                   sts))) ;; 递归进入复合步骤，将没有catcher或没有step的复合步骤合并入步骤列表
    (define more (map (lambda (st) (match (syntax-e st) (`(,prefix ,body ...) #:when (identifier=catch? prefix) (datum->syntax st `(,prefix ,@(optimize-catch-or-steps body)))) (_ st))) appended)) ;; 递归进入catcher
    (filter-not identifier=Right? ;; Right相当于values
                (take-until (dropf-right more (lambda (st) (not (step? st)))) ;; 末尾的catcher没有意义
                            identifier=Left?) ;; Left之后的永远不会执行
                ))

  (define (step? st) (match (syntax-e st) (`(,prefix ,body ...) #:when (identifier=catch? prefix) #f) (_ #t)))
  (define (no-catcher? sts)
    (null? (filter (lambda (st) (not (step? st))) sts)))

  (define (maybe-literalize s)
    (return-if/else s (lambda (s) (pair? (syntax->datum s))) (list 'quote s)))

  (match (syntax-e stx)
    (`(,op ,v ,sts ...)
     #:when (identifier=>>>? op)
     (datum->syntax stx (let/cc cc `(,op ,v ,@(return-if/else (optimize-catch-or-steps sts) (lambda (sts) (not (null? sts))) (cc (maybe-literalize v))))))) ;; 如果没有step，直接返回输入值
    (`(,op ,sts ...)
     #:when (identifier=>>>/steps? op)
     (datum->syntax stx `(,op ,@(optimize-catch-or-steps sts))))
    (_ stx)))
