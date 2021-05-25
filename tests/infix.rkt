#lang racket/base

(module+ test
  (require rackunit
           "../main.rkt")

  ;; infix expressions
  (check-equal? {2 + 3}       5)
  (check-equal? {2 + {3 + 4}} 9))

