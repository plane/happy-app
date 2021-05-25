#lang racket/base

(provide #%app _)

;; --------------------------------------------------------------------------
;;  
;; `happy-app`
;;
;; The macros are defined in a submodule so #%app can be used normally 
;; internally.  The submodule is then `require`d so that it can be tested
;; with rackunit at the bottom, or on the REPL.
;;  
;; --------------------------------------------------------------------------

(module happy-app racket/base
  (provide (rename-out [happy-app #%app])
           _)

  (require
    (for-syntax racket/base
                syntax/parse/class/paren-shape))
  
  (require racket/dict
           racket/function
           racket/list
           racket/sequence
           syntax/parse/define)
  
  (require (only-in ugly-app [#%app ugly-app] _))
  
  (define-syntax-rule 
    (infix-app lhs operator rhs)
    (ugly-app operator lhs rhs))
  
  (define (ref-or-curry arg0 arg1 . rest)
    (cond
      ;; arg0 = container, arg1 = index
      [(and (empty? rest)
            (dict? arg0))     (dict-ref arg0 arg1)]
      [(and (empty? rest)
            (sequence? arg0)) (sequence-ref arg0 arg1)]
  
      ;; arg0 = procedure, arg1 = procedure arg
      ((procedure? arg0) (ugly-app apply curry arg0 arg1 rest))
      [else
       (raise-argument-error 'ref-or-curry
                             "dict? or sequence?"
                             arg0)]))
  
  (define-syntax (arrow-app stx)
    (syntax-parse stx #:datum-literals (-> ->*)
  
      ;; rhs = {curly infix}
      [(_ arg ... ->  (~braces lhs op rhs))     #'(λ (arg ...    ) (op lhs rhs))]
  
      ;; rhs = (application expression)
      [(_ arg ... ->  (~parens expr ...))       #'(λ (arg ...    ) (expr ...))]
      [(_ arg ... ->* (~parens expr ...))       #'(λ (arg ... . _) (expr ...))]
  
      ;; rhs = [curried function]
      [(_ arg ... ->  (~brackets e0 e1 e2 ...)) #'(λ (arg ...    ) (curry e0 e1 e2 ...))]
      [(_ arg ... ->* (~brackets e0 e1 e2 ...)) #'(λ (arg ... . _) (curry e0 e1 e2 ...))]
  
      ;; rhs = [container reference OR curried function]
      [(_ arg ... ->  (~brackets expr ...))     #'(λ (arg ...    ) (ref-or-curry expr ...))]
      [(_ arg ... ->* (~brackets expr ...))     #'(λ (arg ... . _) (ref-or-curry expr ...))]
  
      ;; rhs = applicable expression, but with parens () omitted
      [(_ arg ... ->  expr0 expr1 expr2 ...)    #'(λ (arg ...    ) (expr0 expr1 expr2 ...))]
      [(_ arg ... ->* expr0 expr1 expr2 ...)    #'(λ (arg ... . _) (expr0 expr1 expr2 ...))]
  
      ;; rhs = expression
      [(_ arg ... ->  expr)                     #'(λ (arg ...    ) expr)]
      [(_ arg ... ->* expr)                     #'(λ (arg ... . _) expr)]
  
      ;; one last case so we can support [+] => (curry +)
      [(_ arg)                                  #'(curry arg)]

      ;; container reference OR curried expression
      [(_ arg expr ...)                         #'(ref-or-curry arg expr ...)]))
  
  (define-syntax (happy-app stx)
    ;; we might get paren shapes like (#\{ . #\{),
    ;; but we only care about the #\{ at the beginning
    (define (ca*r v)
      (if (pair? v)
          (ca*r (car v))
          v))
    (define app-stx
      (case (ca*r (syntax-property stx 'paren-shape))
        [(#\{) #'infix-app]
        [(#\[) #'arrow-app]
        [(#f
          #\() #'ugly-app]))
    (syntax-parse stx
      [(_ v ...)
       #`(#,app-stx v ...)])))

(require 'happy-app)

;; --------------------------------------------------------------------------
;;  
;; Tests 
;;  
;; --------------------------------------------------------------------------

(module+ test
  (require rackunit)

  ;; infix expressions
  (check-equal? {2 + 3}       5)
  (check-equal? {2 + {3 + 4}} 9)

  ;; lambdas with _ using ugly-app:
  (check-equal? ((_ 2 3) +    ) 5)
  (check-equal? ((+ _ 3)   2  ) 5)
  (check-equal? ((+ 2 _)     3) 5)
  (check-equal? ((_ _ 3) + 2  ) 5)
  (check-equal? ((_ 2 _) +   3) 5)
  (check-equal? ((+ _ _)   2 3) 5)
  (check-equal? ((_ _ _) + 2 3) 5)
  
  ;; same thing, but using infix expressions
  (check-equal? ({_ + 3} 2    ) 5)
  (check-equal? ({2 _ 3}   +  ) 5)
  (check-equal? ({2 + _}     3) 5)
  (check-equal? ({_ _ 3} 2 +  ) 5)   ;; test fails; FIXME
  (check-equal? ({_ + _} 2   3) 5)
  (check-equal? ({2 _ _}   + 3) 5)
  (check-equal? ({_ _ _} 2 + 3) 5)   ;; test fails; FIXME

  ;; just a value on rhs
  (check-equal? (map [x -> x]       (list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (map [x -> 'x]      (list 1 2 3 4)) (list 'x 'x 'x 'x))

  ;; first with parens on rhs
  (check-equal? (map [x -> (* x)]   (list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (map [x -> (* x x)] (list 1 2 3 4)) (list 1 4 9 16))

  ;; now without parens
  (check-equal? (map [x -> * x]     (list 1 2 3 4)) (list 1 2 3 4))
  (check-equal? (map [x -> * x x]   (list 1 2 3 4)) (list 1 4 9 16))

  ;; discarding extra args
  (check-equal? ([x y ->* (* x y)] 5 6    ) 30)
  (check-equal? ([x y ->* (* x y)] 5 6 7  ) 30)
  (check-equal? ([x y ->* (* x y)] 5 6 7 8) 30)

  ;; now without parens
  (check-equal? ([x y ->* * x y] 5 6    ) 30)
  (check-equal? ([x y ->* * x y] 5 6 7  ) 30)
  (check-equal? ([x y ->* * x y] 5 6 7 8) 30)

  ;; thunks (with empty lhs)
  (check-equal? 
   (call-with-values [-> values 2 3 4]
                     [x y z -> * x y z]) 24)

  ;; thunk*s 
  (check-equal? (build-list 4 [->* 'a])
                (list 'a 'a 'a 'a))

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
