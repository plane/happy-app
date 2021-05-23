Some outstanding issues make this unsuitable as a general-purpose
package for now:

1. No documentation

2. `->` and `->*` lambdas alias the symbols in `(racket/contract)`, and
   no longer work if it is `require`d (even if they seem unambiguous)

3. infix expressions with an initial wildcard `{_ x y}` do not work,
   even though the corresponding ugly-app expressions `(x _ y)` work

But it's a fun toy project (:
