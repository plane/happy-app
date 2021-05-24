Some outstanding issues make this unsuitable as a general-purpose
package for now:

1. `->` and `->*` lambdas alias the symbols in `(racket/contract)`, and
   no longer work if it is `require`d (even if they seem unambiguous)

2. Needs documentation

3. infix expressions with an initial wildcard `{_ x y}` mostly work now,
   thanks to Alex Knauth's help, although two test cases still fail

4. Code in general could be cleaned up
