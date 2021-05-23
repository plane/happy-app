#lang racket/base

(provide container-ref)

;; --------------------------------------------------------------------------

(require racket/dict
         racket/sequence)

(define (container-ref container index)
  (cond
    [(dict? container)     (dict-ref container index)]
    [(sequence? container) (sequence-ref container index)]
    [else
     (raise-argument-error 'container-ref
                           "dict? or sequence?"
                           container)]))

