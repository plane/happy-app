#lang racket/base

(module+ test
  (require rackunit
           "../main.rkt")

  ;; just a value on rhs
  (check-equal? (map [x -> x]       (list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (map [x -> 'x]      (list 1 2 3 4)) (list 'x 'x 'x 'x))

  ;; first with parens on rhs
  (check-equal? (map [x -> (* x)]   (list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (map [x -> (* x x)] (list 1 2 3 4)) (list 1 4 9 16))

  ;; same, but with implied parens
  (check-equal? (map [x -> * x]     (list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (map [x -> * x x]   (list 1 2 3 4)) (list 1 4 9 16))

  ;; discarding extra args with ->*
  (check-equal? ([x y ->* (* x y)] 5 6    ) 30)
  (check-equal? ([x y ->* (* x y)] 5 6 7  ) 30)   ; 7 discarded
  (check-equal? ([x y ->* (* x y)] 5 6 7 8) 30)   ; 7 8 discarded

  ;; same, but with implied parens
  (check-equal? ([x y ->* * x y] 5 6    ) 30)
  (check-equal? ([x y ->* * x y] 5 6 7  ) 30)
  (check-equal? ([x y ->* * x y] 5 6 7 8) 30))

