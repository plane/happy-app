#lang racket/base

(module+ test
  (require rackunit
           "../main.rkt")

  ;; dict-ref
  (check-equal? [(hash 'color 'pink
                       'shape 'rhombus) 'color] 'pink)

  ;; sequence-ref
  (check-equal? ['(hello world) 1] 'world)
  (check-equal? ['(a b c d e) 3] 'd)
  
  ;; some curried funcs
  (check-equal? (map [+ 2]          (list 1 2 3 4)) (list 3 4 5 6))
  (check-equal? (map [+ 2 3]        (list 1 2 3 4)) (list 6 7 8 9))

  ;; curry with no args
  (check-equal? (([+] 3) 4) (+ 3 4)))

