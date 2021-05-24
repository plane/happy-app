Some outstanding issues make this unsuitable as a general-purpose
package for now:

1. `->` and `->*` lambdas alias the symbols in `(racket/contract)`, and
   no longer work if it is `require`d (even if they seem unambiguous)

2. Needs documentation

3. Needs test suite

4. infix expressions with an initial wildcard `{_ x y}` do work now,
   thanks to Alex Knauth's help, but the implementation may need work

5. Code in general could be cleaned up
