#lang racket/base

(module+ test
  (require rackunit
           "../main.rkt")

  ;; lambdas with _ using ugly-app:
  (check-equal? ((_ 2 3) +    ) 5)
  (check-equal? ((+ _ 3)   2  ) 5)
  (check-equal? ((+ 2 _)     3) 5)
  (check-equal? ((_ _ 3) + 2  ) 5)
  (check-equal? ((_ 2 _) +   3) 5)
  (check-equal? ((+ _ _)   2 3) 5)
  (check-equal? ((_ _ _) + 2 3) 5))
  
