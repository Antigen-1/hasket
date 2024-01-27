(module test "../main.rkt"
  (require rackunit racket/pretty racket/list racket/runtime-path syntax/parse/define (for-syntax "../main.rkt"))
  (define-syntax-parse-rule (print-and-test form pred)
    (begin
      (pretty-write 'form)
      (pred (time form))))

  (module list "../main.rkt"
    (provide check-current)
    (define (check-current p l)
      (findf (lambda (p1) (or (= (cdr p) (cdr p1))
                              (= (abs (- (car p) (car p1)))
                                 (abs (- (cdr p) (cdr p1))))))
             l)))
  (require 'list)
  (define-runtime-module-path-index list-lib '(submod "." list))

  (displayln "Eight queens")
  (displayln "Optimal")
  (print-and-test
   (amb-begin
    #:extensions ((module-path-index-resolve list-lib))
    (let cons/cached cons)
    (let check/cached check-current)
    (let reverse/cached reverse)
    (let car/cached car)
    (let cdr/cached cdr)
    (let null/cached null)
    (let null?/cached null?)
    (let list/cached list)
    (let make-queen (lambda (n) (amb (cons/cached n 1) (cons/cached n 2) (cons/cached n 3) (cons/cached n 4) (cons/cached n 5) (cons/cached n 6) (cons/cached n 7) (cons/cached n 8))))
    (let make-and-merge (lambda (n o) (let q (make-queen n)) (if (check/cached q o) (amb) (cons/cached q o))))
    (let foldl (lambda (p o l) (if (null?/cached l) o (foldl p (p (car/cached l) o) (cdr/cached l)))))
    (reverse/cached (foldl make-and-merge null/cached (list/cached 1 2 3 4 5 6 7 8))))
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
