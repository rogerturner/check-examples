# check-examples

A Chez Scheme library providing `(example: (fn args) => result)` syntax for in-line documentation and simple tests. Usage:
```
(import (check-examples))

(define (square n) ;; Number -> Number
  ;; produce n squared
  (example: (square 5) => 25 )
  (* n n))
  
(check-examples)
```
See examples.ss for more examples.