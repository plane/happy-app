#lang info

(define collection "happy-app")
(define pkg-authors '("crystal@panix.com"))
(define version "0.3")
(define scribblings
 '(("happy-app.scrbl" () (library) "happy-app")))
(define pkg-desc "Happy little #%app enhancement")
(define deps '("base"
               "git://github.com/AlexKnauth/ugly-app?path=ugly-app"
               "syntax-classes-lib"))
(define build-deps '("rackunit-lib"
                     "racket-doc"
                     "scribble-lib"))
(define compile-omit-paths '("examples"))

