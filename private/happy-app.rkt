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
         syntax/parse
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
    [(ref-lambda-app arg ... ->  (~braces lhs op rhs))  #'(λ (arg ...    ) (op lhs rhs))]
    [(ref-lambda-app arg ... ->  (~parens expr ...))    #'(λ (arg ...    ) (expr ...))]
    [(ref-lambda-app arg ... ->* (~parens expr ...))    #'(λ (arg ... . _) (expr ...))]
    [(ref-lambda-app arg ... ->  (~brackets e0 e1 e2 ...))  #'(λ (arg ...    ) (curry e0 e1 e2 ...))]
    [(ref-lambda-app arg ... ->* (~brackets e0 e1 e2 ...))  #'(λ (arg ... . _) (curry e0 e1 e2 ...))]
    [(ref-lambda-app arg ... ->  (~brackets expr ...))  #'(λ (arg ...    ) (ref-or-curry expr ...))]
    [(ref-lambda-app arg ... ->* (~brackets expr ...))  #'(λ (arg ... . _) (ref-or-curry expr ...))]
    [(ref-lambda-app arg ... ->  expr0 expr1 expr2 ...) #'(λ (arg ...    ) (expr0 expr1 expr2 ...))]
    [(ref-lambda-app arg ... ->* expr0 expr1 expr2 ...) #'(λ (arg ... . _) (expr0 expr1 expr2 ...))]
    [(ref-lambda-app arg ... ->  expr)                  #'(λ (arg ...    ) expr)]
    [(ref-lambda-app arg ... ->* expr)                  #'(λ (arg ... . _) expr)]
    [(ref-lambda-app arg expr ...)                      #'(ref-or-curry arg expr ...)]))

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
