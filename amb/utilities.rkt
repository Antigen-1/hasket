(module utils "../base/main.rkt"
  (require (for-template "../base/main.rkt"))
  (provide (all-defined-out))

  ;; Utilities
  (define identifier-symbol=?
    (lambda/curry/match
     ((id1 id2)
      (eq? (syntax->datum id1) (syntax->datum id2)))))
  (define identifier=?
    (lambda/curry/match
     ((base v)
      (and (identifier? v) (free-identifier=? base v)))))
  (define wrap-expr
    (lambda/curry/match
     (((and sts `(,_ ,_ ,@_)))
      ;; 打包为一个整体
      #`(let () #,@sts))
     ((`(,stx)) stx)))
  (define ((make-maybe-wrap-name name) stx)
    ;; 支持递归使用的工具函数
    (if name
        #`(letrec ((#,name #,stx))
            #,name)
        stx))
  (define (wrap-unitL stx)
    #`(#%app unitL #,stx))
  )
