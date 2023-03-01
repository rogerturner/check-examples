#! /usr/local/bin/scheme --script

;; Top-level program using the check-examples library

(import (for (check-examples) expand run))

(define (double n) ;; this example: will be checked but not counted
  (example: (double 2) => 4 )
  (+ n n))

(alias ^example: example:)

(define count 0)

(define-syntax example:
  (syntax-rules (=>)
    [(_ e)      (set! count (+ 1 count)) (^example: e) ]
    [(_ e => r) (set! count (+ 1 count)) (^example: e => r) ]))

(define (square n)
  (example: (square 5)    => 25 )
  (example: (square -1/2) => 1/4 )
  (* n n))

(example:
  (let-values ([(q r) (div-and-mod 17.5 3)])
    (and (eqv? q 5.0) (eqv? r 2.5))) )

(check-examples)

(display count) (display " examples checked\n") (exit)
