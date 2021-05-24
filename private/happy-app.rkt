#lang racket/base

(provide (rename-out [happy-app #%app])
         _)

;; --------------------------------------------------------------------------

(require
  (for-syntax racket/base
              syntax/parse/class/paren-shape))

(require racket/dict
         racket/function
         racket/list
         racket/sequence
         ;         syntax/parse
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

    ;; container reference OR curried expression
    [(_ arg expr ...)                         #'(ref-or-curry arg expr ...)]))

(define-syntax (happy-app stx)
  (define app-stx
    (case (syntax-property stx 'paren-shape)
      [(#\{) #'infix-app]
      [(#\[) #'arrow-app]
      [(#f
        #\() #'ugly-app]))
  (syntax-parse stx
    [(_ v ...)
     #`(#,app-stx v ...)]))

