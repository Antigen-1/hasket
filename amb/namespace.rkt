(module namespace "../base/main.rkt"
  (require racket/runtime-path)
  (provide make-hasket-base-namespace namespace-require! namespace-refer)

  (define-runtime-module-path-index base "../base/main.rkt")
  (define-namespace-anchor anchor)

  (define namespace-require!
    (lambda/curry/match
     ((index src dest)
      (namespace-attach-module src
                               (module-path-index-resolve index)
                               dest)
      (parameterize ((current-namespace dest))
        (namespace-require (module-path-index-resolve index))))))
  (define (namespace-refer ns var name)
    (namespace-variable-value var #t (lambda () (raise-syntax-error name (format "Unbound variable ~a is used" var))) ns))

  (define (make-hasket-base-namespace)
    (define base-ns (make-empty-namespace))
    (namespace-require! base (namespace-anchor->empty-namespace anchor) base-ns)
    base-ns))
