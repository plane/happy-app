#lang racket/base

(provide (rename-out [happy-app #%app])
         _)

;; --------------------------------------------------------------------------

(require syntax/parse/define
         (for-syntax racket/base))

(require (only-in ugly-app [#%app ugly-app] _)
         "infix-app.rkt"
         "ref-lambda-app.rkt")

(define-syntax (happy-app stx)
  (define app-stx
    (case (syntax-property stx 'paren-shape)
      [(#\{) #'infix-app]
      [(#\[) #'ref-lambda-app]
      [(#f
        #\() #'ugly-app]))
  (syntax-parse stx
    [(_ v ...)
     #`(#,app-stx v ...)]))
