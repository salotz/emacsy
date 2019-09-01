;; Monad --- Manage state without too much mutation.

;; Copyright (C) 2019 Amar Singh

;; This file is part of Emacsy.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (emacsy monad)
  #:use-module (srfi srfi-1)
  #:use-module (emacsy circular)
  #:export (
            monadic
            fire
            m
            numeric-monad
            ))

;;; the general global var pattern, mutate the state until you have the
;;; thing you want. History is forgotten by mutation.
(define-public value 0)

(define-public (adder . rest)
  (set! value (apply + value rest))
  value)

(define-public (tractor . rest)
  (set! value (apply - value rest))
  value)

;;; do the same thing without mutation.
(define-public cvalue 0)

(define-public add +)

(define-public tract -)

;;; i want to write something like (monadic (add 1) (tract 3)) etc
(define-public (monadic start . mthunk)
  (define (sequencer thunk start)
    (if (null? thunk) start
        (cons (car thunk)
              (sequencer (cdr thunk) start))))
  (define (seq thunks start)
    (if (null? thunks) start
        (seq (cdr thunks) (list (append (car thunks) start)))))
  (car (seq mthunk (list start))))

(define-public numeric-monad (partial monadic 0))

;;; usage: (fire (monadic (m proc1 arg1) (m proc2 arg2)))
(define-public (fire monadic)
  (eval monadic (interaction-environment)))

(define-public (m proc val)
  (list proc val))
