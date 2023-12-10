#lang hasket
(require rackunit)

(define tree '(a (b) c))
(define a (lambda (v) (Right (add1 v))))
(define b (lambda/curry/match
           (((errorR (exn str cm) position))
            (define x (string->number str))
            (if (zero? (remainder x 2)) (Right x) (Left (exn str cm))))))
(define c (lambda (v) (if (zero? (remainder v 3)) (Right v) (Left (exn (format "~a" v) (current-continuation-marks))))))
(define (refer tree p)
  (foldl (lambda (i t) (list-ref t i)) tree p))
(define r (lambda (v)
            (define s (>>> v a ($ b) c))
            (cond ((errorR? s)
                   (list (refer tree (errorR-position s)) (exn-message (errorR-exception s))))
                  (else (list #f s)))))

(check-equal? (r 0) '(b "1"))
(check-equal? (r 2) '(#f 3))
(check-equal? (r 5) '(#f 6))