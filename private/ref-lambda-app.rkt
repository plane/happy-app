#lang racket/base

(provide ref-lambda-app)

;; --------------------------------------------------------------------------

(require syntax/parse/define
         (for-syntax racket/base))

(require "container-ref.rkt")

(define-syntax (ref-lambda-app stx)
  (syntax-parse stx #:datum-literals (-> ->*)
    [(ref-lambda-app arg ... ->* expr ...) #'(lambda (arg ... . _) expr ...)]
    [(ref-lambda-app arg ... ->  expr ...) #'(lambda (arg ...    ) expr ...)]
    [(ref-lambda-app container index)      #'(container-ref container index)]))

