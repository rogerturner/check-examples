;; © 2023 Roger Turner <https://github.com/rogerturner/check-examples/issues/new/choose>
;; SPDX-License-Identifier: LGPL-3.0-or-later  (see Notices below)

#| _minimal_ check-examples library for Chez Scheme. Usage:
(define (square n) ;; Number -> Number
  ;; produce n squared
  (example: (square 5) => 25 )
  (* n n))
  
(check-examples)
|#

#!chezscheme

(library (check-examples)

(export example: check-examples)

(import (chezscheme))

(meta define *examples* (list))

(define-syntax example:
  (lambda (x)
    (set! *examples*
      (cons
        (syntax-case x (=>)
          [(example: e)      #'e ]
          [(example: e => r) #'(equal? e r) ])
        *examples*))
    #'(define _ #f)))

(define-syntax check-examples
  (lambda (_)
    (syntax-case _ ()
      [(_) #`(begin
            #,@(map (lambda (ex)
                  #`(assert #,ex))
                *examples*) (void)) ])))

)

#| Notices:

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published
  by the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
  
  License: <https://spdx.org/licenses/LGPL-3.0-or-later.html>
  Contact: <https://github.com/rogerturner/check-examples/issues/new/choose>  |#
