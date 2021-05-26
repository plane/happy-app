#lang info

(define collection "happy-app")
(define pkg-authors '("crystal@panix.com"))
(define version "0.6")
(define scribblings
 '(("happy-app.scrbl" () (library) "happy-app")))
(define pkg-desc "Happy little #%app enhancement")
(define deps '("base"
               "ugly-app-lib"
               "syntax-classes-lib"))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "scribble-lib"))
(define compile-omit-paths '("examples"))

