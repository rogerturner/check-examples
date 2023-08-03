#! /usr/local/bin/scheme --script
;; SPDX-FileCopyrightText:  © 2023 Roger Turner <r@rogerturner.com>
;; SPDX-License-Identifier: LGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| Executable Chez Scheme top-level program.
   Example uses of the check-examples library:
     extend |example:|, with examples of use
     extend |check-examples| for use with..
     ..simple REPL that checks examples after every input  |#

(import (check-examples))

(alias ^example: example:)

(define-syntax example:
  ;; extend |example:| with binding option, ==> for eq? test
  (lambda (ex)
    (syntax-case ex (=> ==>)
      [(_ (let~ bindings e => r))
                     #'(^example: (let~ bindings (equal? e r))) ]
      [(_ (let~ bindings e ==> r))
                     #'(^example: (let~ bindings (eq? e r))) ]
      [(_ e ==> r)   #'(^example: (eq? e r)) ]
      [(_ arg ...)   #'(^example: arg ...) ])))

(define (square n)
  (example: (square 5) => 25 )
  (* n n))

(example: (let-values ([(ts fs) (partition odd? '(1 2 3))])
            (list ts fs))
          => '((1 3) (2)) )

(define-syntax incr!
  (lambda (stx)
    (example: (define y 10)
              (incr! y)
              y             => 11 )
    (syntax-case stx ()
      [(_ x) #'(set! x (+ x 1))])))

(define (list-reverse! xs)
  ;; produce reverse of proper list xs, *mutating xs*
  (example: (list-reverse! (list 1 2 3)) => '(3 2 1) )
  (example: (let* ([xs (list 1 2 3)] [x3 (cddr xs)])
              (list-reverse! xs)
              ==> x3 ))
  (example: (let* ([xs (list 1 2 3)] [_ (list-reverse! xs)])
              xs)
            => '(1) )
  ;; (list-reverse! '(1 2 3)) <- Don't do this!
  (let next-x ([xs xs] [rs (list)])
    (if (null? xs)  rs
        (let ([rest (cdr xs)])
          (set-cdr! xs rs)
          (next-x rest xs)))))

(check-examples)

(for-each display `(
    "> (example: (foo bar) => qux) © 2023 Roger Turner\n"
    ,(let-syntax ([n (lambda _ (length *examples*))]) n)" examples checked\n"))

(meta begin (set-examples (list)))


#| minimal REPL that maintains a list of examples, checked after every eval;
   failing examples are removed. Sample interaction:
    % use-check-examples.ss
    > (example: (square 5) => 25)
    1 correct example
    > (example: (double 2) => 4)  ; (will be checked after definition of double)
    Exception: variable double is not bound
    Type (debug) to enter the debugger.
    > (define (double n) (+ n n))
    2 correct examples
    > (example: (square (sqrt 2)) => 2)  ; (sqrt 2) produces a Flonum
    **example error: (equal? (let () (square (sqrt 2))) 2)
    >
|#

(meta define *correct-examples* 0)  ;; count of previous correct examples

(meta define (remove-example ex)
  (set-examples (remq ex *examples*)))

(define-syntax check-examples
  ;; produce syntax to display count (having eval'd examples)
  (lambda (_)
      (with-exception-handler
        (lambda (ex)
          (unless (memq ex *examples*) (raise ex))
          (for-each display `("**example error: " ,(syntax->datum ex) "\n"))
          (remove-example ex))
        (lambda ()
          (for-each (lambda (ex)
              (unless (eval (syntax->datum ex))
                (raise-continuable ex)))
            (reverse *examples*))
          (let ([ce (length *examples*)])
            (cond
              [(= ce *correct-examples*) #'(void) ]
              [else
                (set! *correct-examples* ce)
                #`(for-each display `(#,ce
                    " correct example" #,(if (= 1 ce) "" "s") "\n")) ]))))))

(new-cafe
  (lambda (x)
    (let ([r (eval x)])
      (eval '(check-examples))
      r)))


#| *Notices*

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.

  License: <https://spdx.org/licenses/LGPL-3.0-or-later.html>
  Contact: <https://github.com/rogerturner/Contact/issues/new/choose>  |#
