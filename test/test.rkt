(module test "../main.rkt"
  (require rackunit)

  (define tree '(a (b) c))
  (define a (Right . add1))
  (define b (lambda/curry/match
             (((errorR (at (exn str cm) _)))
              (define x (string->number str))
              (if (zero? (remainder x 2)) (Right x) (Left (exn str cm))))))
  (define c (lambda (v) (if (zero? (remainder v 3)) (Right v) (Left (exn (format "~a" v) (current-continuation-marks))))))
  (define (refer tree p)
    (foldl (lambda (i t) (list-ref t i)) tree p))
  (define r (lambda (v)
              (define s (>>> v (>>>/steps a ($ b) c)))
              (cond ((errorR? s)
                     (list (refer tree (at-position (errorR-value s))) (exn-message (at-value (errorR-value s)))))
                    (else (list #f s)))))

  (check-equal? (r 0) '(b "1"))
  (check-equal? (r 2) '(#f 3))
  (check-equal? (r 5) '(#f 6))

  (check-true (>>> 0 (lambda (_) ((mapM zero?) (Right 0)))))
  (check-true (>>> 0 (lambda (_) (joinM (Right (Right #t))))))

  (check-true (zero? (((curry/n - 2) 1) 1)))

  (struct A (v) #:transparent #:methods gen:monad [(define (mapM f m) (A (f (A-v m))))])
  (check-equal? (mapM add1 (A 1)) (A 2))
  )
