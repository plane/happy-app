#lang racket/base

(provide infix-app)

;; --------------------------------------------------------------------------
  
(require (only-in ugly-app [#%app ugly-app]))

(define-syntax-rule 
  (infix-app lhs operator rhs)
  (ugly-app operator lhs rhs))

