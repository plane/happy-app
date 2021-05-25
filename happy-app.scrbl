#lang scribble/manual

@title{Happy App: A Toolbox of Syntactic Shortcuts}
@defmodule[happy-app]
@author[@author+email["Plane" "crystal@panix.com"]]

@(require
   scribble/example
   (for-label (except-in racket/base #%app _)
              racket/function
              happy-app))

@(define (make-happy-eval)
   (make-base-eval #:lang 'racket/base
                   '(require happy-app)
                   '(random-seed 0)))

@(define-syntax-rule (happy-examples body ...)
   (examples #:eval (make-happy-eval) #:preserve-source-locations body ...))

This package provides a whole bunch of syntactic shortcuts by redefining 
@racket[#%app].  It makes me happy, and I hope it makes you happy too!

@section{Lambdas with placeholders}

Lambdas like this:

@racketblock[(lambda (x) 
               (+ x 1))]

can be shortened to this:

@racketblock[(+ _ 1)]

Any parenthesized expression with at least one @racket[_] placeholder will be
automatically converted into a lambda expression. 

@(happy-examples
  (eval:check (map (+ 1 _) (list 1 2 3 4)) '(2 3 4 5))
  (eval:check (map (+ _ 2) (list 1 2 3 4)) '(3 4 5 6)))

The placeholder can be in any position, even initial position:

@(happy-examples
  (eval:check (map (_ 2 3) (list + - * /)) '(5 -1 6 2/3)))

This functionality is taken directly from 
@hyperlink["https://github.com/AlexKnauth/ugly-app"]{Alex Knauth's @code{ugly-app} package}.

@section{Arrow lambdas with @racket[[x -> x]]}

Lambdas like this:

@racketblock[(lambda (x) 
               (+ x 1))]

can be written this way:
@racketblock[[x -> (+ x 1)]]

These are called "arrow lambdas".  Function arguments go on the left side, 
and an expression to evaluate goes on the right side.  

@(happy-examples
  (eval:check
   (map [x -> x] '(1 2 3 4))
   '(1 2 3 4))
  (eval:check
   (map [x -> (* x x)] '(1 2 3 4))
   '(1 4 9 16))
  (eval:check
   (map [x -> * x x] '(1 2 3 4)) 
   '(1 4 9 16))
  (eval:check 
   (map [x y z -> (x y z)]
        (list + - * /)
        (list 1 2 3 4)
        (list 4 5 6 7))
   '(5 -3 18 4/7)))

You can leave out the parentheses if there's more than one term on the right, 
so the following examples are equivalent:

@racketblock[[x -> (+ x 1)]]
@racketblock[[x -> + x 1]]

Parentheses can't be omitted from single-term expressions, because there's
no way to figure out if you mean @racket[[x -> x]] or @racket[[x -> (x)]].

Inspired by Haskell's @code{\x -> x} lambda syntax.

@section{Arrow lambdas with @racket[[x ->* x]] (discarding extra arguments)}

These are the same as with @racket[->], but any extra arguments are discarded.

@(happy-examples
  (eval:check ([x ->* x] 'hello 'world) 'hello))

In the examples below, the lambda takes two arguments @racket[x] and @racket[y]; 
any extra arguments you pass in get thrown away.

@(happy-examples
  (eval:check ([x y ->* (* x y)] 5 6) 30)
  (eval:check ([x y ->* (* x y)] 5 6 7) 30)
  (eval:check ([x y ->* (* x y)] 5 6 7 8) 30))

Just like with @racket[->], parentheses can be omitted in multi-term expressions:

@(happy-examples
  (eval:check ([x y ->* + x y] 5 6 7) 11))

@section{Thunks with @racket[[-> x]]}

These are simply arrow lambdas with no arguments (nullary functions).  Rather
than writing out a lambda like this:

@racketblock[(lambda () (+ 1 2 3))]

You can shorten it to this:

@racketblock[[-> (+ 1 2 3)]]

And just like before, you can leave out the parentheses:

@racketblock[[-> + 1 2 3]]

@(happy-examples
  (eval:check
    (call-with-values [-> values 2 3 4]
                      [x y z -> * x y z])
    24))

This function is like @racket[thunk] from @racketmodname[racket/function].  

@section{Thunk*s with @racket[[->* x]]}

Like the above, but the thunk accepts arguments and discards them.

@(happy-examples
  (eval:check
    (build-list 10 [->* (random 100)])
    '(85 65 20 40 89 45 54 38 26 62)))

This function is like @racket[thunk*] from @racketmodname[racket/function].  

@section{Container access with @racket[[container index]]}

@(happy-examples
  (eval:check ['(hello world) 1] 'world)
  (eval:check [(hash 'color 'pink
                     'shape 'rhombus) 'color] 'pink))

Inspired by @hyperlink["https://github.com/greghendershott/rackjure"]{Greg Hendershott's Rackjure}.

@section{Curried functions with @racket[[]]}

@(happy-examples
  (eval:check (map [+ 5] '(1 2 3 4)) '(6 7 8 9)))

The choice between @racket[[+ 5]] and @racket[[sequence index]] is decided at run-time,
because there's no syntactic difference between the two.  Although this should usually be
unambiguous, @code{happy-app} checks @racket[dict?] first, then @racket[sequence?], then 
@racket[procedure?]; ambiguous cases will be resolved in that order.

This function is like @racket[curry] from @racketmodname[racket/function].  

@section{Binary infix expressions with @racket[{}]}

@(happy-examples
  (eval:check
    (map [x -> {x * x}]
         '(4 5 6 7))
    '(16 25 36 49)))

Inspired by @hyperlink["https://srfi.schemers.org/srfi-105/srfi-105.html"]{David Wheeler's SRFI 105}.

