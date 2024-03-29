(module test "../main.rkt"
  (require rackunit racket/pretty racket/list syntax/parse/define (for-syntax "../main.rkt"))
  (define-syntax-parse-rule (print-and-test form pred)
    (begin
      (pretty-write 'form)
      (pred (time form))))

  (define (check-current p l)
    (findf (lambda (p1) (or (= (cdr p) (cdr p1))
                            (= (abs (- (car p) (car p1)))
                               (abs (- (cdr p) (cdr p1))))))
           l))

  (displayln "Eight queens")
  (displayln "Optimal")
  (print-and-test
   (amb-begin
    (let make-queen
        (lambda (n)
          (let m (amb 1 2 3 4 5 6 7 8))
          (cons n m)))
    (let make-and-merge (lambda (n o) (let q (make-queen n)) (if (check-current q o) (amb) (cons q o))))
    (reverse (amb-foldl make-and-merge null '(1 2 3 4 5 6 7 8))))
   (lambda (l1)
     (displayln "Typical")
     (print-and-test
      (let* ((make-queen (lambda (n) (map (lambda (m) (cons n m)) (list 1 2 3 4 5 6 7 8))))
             (ns (list 1 2 3 4 5 6 7 8))
             (queens (map make-queen ns))
             (conditions (apply cartesian-product queens)))
        (filter (lambda/curry/match
                 #:name checker
                 (((cons car-v cdr-v))
                  (if (check-current car-v cdr-v)
                      #f
                      (checker cdr-v)))
                 (((list))
                  #t))
                conditions))
      (lambda (l2) (check-equal? l1 l2))))))
