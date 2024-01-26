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
  (displayln "Several instructions are cached")
  (print-and-test
   (amb-begin
    #:extensions ((module-path-index-resolve list-lib))
    (let cons/cached cons)
    (let check/cached check-current)
    (let list/cached list)
    (let reverse/cached reverse)
    (let make-queen (lambda (n) (amb (cons/cached n 1) (cons/cached n 2) (cons/cached n 3) (cons/cached n 4) (cons/cached n 5) (cons/cached n 6) (cons/cached n 7) (cons/cached n 8))))
    (let q1 (make-queen 1))
    (let result0 (list/cached q1))
    (let q2 (make-queen 2))
    (if (check/cached q2 result0)
        (amb)
        (begin
          (let result1 (cons/cached q2 result0))
          (let q3 (make-queen 3))
          (if (check/cached q3 result1)
              (amb)
              (begin
                (let result2 (cons/cached q3 result1))
                (let q4 (make-queen 4))
                (if (check/cached q4 result2)
                    (amb)
                    (begin
                      (let result3 (cons/cached q4 result2))
                      (let q5 (make-queen 5))
                      (if (check/cached q5 result3)
                          (amb)
                          (begin
                            (let result4 (cons/cached q5 result3))
                            (let q6 (make-queen 6))
                            (if (check/cached q6 result4)
                                (amb)
                                (begin
                                  (let result5 (cons/cached q6 result4))
                                  (let q7 (make-queen 7))
                                  (if (check/cached q7 result5)
                                      (amb)
                                      (begin
                                        (let result6 (cons/cached q7 result5))
                                        (let q8 (make-queen 8))
                                        (if (check/cached q8 result6)
                                            (amb)
                                            (begin
                                              (let result7 (cons/cached q8 result6))
                                              (reverse/cached result7))))))))))))))))
   (lambda (l1)
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
