Some outstanding issues make this unsuitable as a general-purpose
package for now:

1. No docs

2. `->` and `->*` alias the symbols in `(racket/contract)`, and stop
   working if it is required (even if they seem unambiguous)

3. infix expressions with an initial wildcard `{_ x y}` do not work,
   even though the corresponding ugly-app expressions `(x _ y)` work

