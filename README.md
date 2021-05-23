# happy-app

Various enhancements to #%app

1. lambdas with `_` using `ugly-app`:

   ```scheme
   (map (_ 2 3) (list + - * /))             ; '(5 -1 6 2/3)
   ```
   
   Requires ugly-app to be installed:  
   https://github.com/AlexKnauth/ugly-app

2. lambdas with `[x -> x]`:

   ```scheme
   (map [x y z -> (x y z)]
        (list + - * /)
        (list 1 2 3 4)
        (list 4 5 6 7))                     ; '(5 -3 18 4/7)
   ```

   Inspired by Haskell's `\x -> x` lambda syntax

3. lambdas with `[x ->* x]` (discarding extra arguments):

   ```scheme
   ([x y ->* (* x y)] 5 6 7)                ; 30
   ```

4. thunks with `[-> x]`:

   ```scheme
   (call-with-values [-> (values 2 3 4)]
                     [x y z -> (* x y z)])  ; 24
   ```

   Like `thunk` in `(racket/function)`

5. thunks with `[->* x]` (discarding any arguments):

   ```scheme
   (build-list 10 [->* (random)])           ; list of random values
   ```

   Like `thunk*` in `(racket/function)`

6. `dict-ref` and `sequence-ref` with `[]`:

   ```scheme
   ['(hello world) 1]                       ; 'world
   [(hash 'color 'pink
          'shape 'rhombus) 'color]          ; 'pink
   ```

   Inspired by Greg Hendershott's Rackjure:  
   https://github.com/greghendershott/rackjure

7. curried functions with `[]`:

   ```scheme
   (map [+ 5] '(1 2 3 4))                   ; '(6 7 8 9)
   ```

   Like `curry` in `(racket/function)`

8. binary infix expressions with `{}`:

   ```scheme
   (map [x -> {x * x}]
        '(4 5 6 7))                         ; '(16 25 36 49)
   ```

   Inspired by David Wheeler's SRFI 105:  
   https://srfi.schemers.org/srfi-105/srfi-105.html

See BUGS.md
