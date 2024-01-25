(module test "../main.rkt"
  (require rackunit racket/pretty racket/runtime-path syntax/parse/define (for-syntax "../main.rkt"))
  (define-syntax-parse-rule (print-and-test form actual)
    (begin
      (pretty-write 'form)
      (check-equal? (time form) actual)))

  (module list "../main.rkt"
    (provide check)
    (define (check p l)
      (findf (lambda (p1) (or (= (car p) (car p1))
                              (= (cdr p) (cdr p1))
                              (= (abs (- (car p) (car p1))) (abs (- (cdr p) (cdr p1))))))
             l)))
  (define-runtime-module-path-index list-lib '(submod "." list))

  (displayln "Eight queens")
  (displayln "Several instructions are cached")
  (print-and-test
   (length
    (amb-begin
     #:extensions ((module-path-index-resolve list-lib))
     (let cons/cached cons)
     (let check/cached check)
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
                                             (reverse/cached (cons/cached q8 result6)))))))))))))))))
   92))
