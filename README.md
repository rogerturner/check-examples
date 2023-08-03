# check-examples

A _**lightweight**_ Chez Scheme library providing `(example: (fn arg/s) => result)` syntax for in-line documentation and simple tests. Usage:
```
(import (check-examples))

(example: (+ 2 2) => 4 )

(define (square n) ;; Number -> Number
  ;; produce n squared
  (example: (square 5) => 25 )
  (* n n))

(check-examples)
```
See use-check-examples.ss for more examples.
