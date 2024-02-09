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
      `(,#'let () ,@sts))
     ((`(,stx)) stx)
     ((v) (raise-syntax-error #f "No expression" v))))
  (define (wrap-unitL stx)
    #`(#%app unitL #,stx))
  )
