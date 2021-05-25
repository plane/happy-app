#lang racket/base

(module+ test
  (require rackunit
           "../main.rkt")

  ;; thunks (with empty lhs)
  (check-equal? 
   (call-with-values [-> values 2 3 4]
                     [x y z -> * x y z]) 24)

  ;; thunk*s, discarding any arguments
  (check-equal? (build-list 4 [->* 'a])
                (list 'a 'a 'a 'a)))

