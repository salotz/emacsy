;;; <file:mru-stack-test.scm>=
;;; @subsection Legal Stuff
;;;
;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;;
;;; This file is part of Emacsy.
;;;
;;; Emacsy is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; Emacsy is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with Emacsy.  If not, see <http://www.gnu.org/licenses/>.
(use-modules (emacsy mru-stack)
             (check))

(use-private-modules (emacsy mru-stack))
;;; <mru-stack:test>=
;;; test for input and output types for all procs.

;;; sanity check
(define s (make <mru-stack>))
(for-each (lambda (x)
            (set! s (mru-add s x)))
          '(a b c))
(check (mru->list s) => '(c b a))
(check (mru->list (mru-recall s 'a)) => '(a c b))
(set! s (mru-recall s 'a))
(check (mru-ref s) => 'a)
(set! s (mru-next s))
(check (mru-ref s) => 'c)
(set! s (mru-next s))
(check (mru-ref s) => 'b)
(set! s (mru-next s))
(check (mru-ref s) => 'a)
(set! s (mru-prev s))
(check (mru-ref s) => 'b)
(set! s (mru-remove s 'c))
(check (mru-ref s) => 'b)
(set! s (mru-remove s 'a))
(set! s (mru-remove s 'b))
(check (mru->list s) => '())
(check (mru-ref s) => #f)
(set! s (mru-next s))
(check (mru-ref s) => #f)
(set! s (mru-add s 'a))
(set! s (mru-add s 'b))
(set! s (mru-add s 'c))
(check (mru->list s) => '(c b a))
(set! s (mru-remove s 'c))
(check (mru->list s) => '(b a))
(check (mru-ref s) => 'b)
(set! s (mru-remove s 'b))
(set! s (mru-remove s 'a))
(for-each (lambda (x)
            (set! s (mru-add s x)))
          '(a b c))
(set! s (mru-next s))
(set! s (mru-remove s 'a))
(set! s (mru-remove s 'b))
(set! s (mru-remove s 'c))
(check (mru->list s) => '())

;;; check top level constants
(check ROF => #f)
(check LIST-BEGIN => 0)
(check INCR => 1)

;;; check mru object
(check  (q s) => '())
(check (q (make <mru-stack> #:q '(in the end))) => '(in the end))

;;; check write
(check (call-with-output-string (lambda (port) (write s port))) => "<mru-stack ()>")

;;; check procs
(check (q (mru-add s 'a)) => '(a))
(check (q (mru-add (make <mru-stack>) #f)) => '(#f))

;;; remove only one element, though we do not like having eq? elements
;;; in mru, if somehow a user managed to get eq? elements inside mru,
;;; atleast let him do as he pleases.
(check (q (mru-remove (make <mru-stack> #:q '(a a b b c c)) 'a)) => '(a b b c c))
(check (q (mru-remove (make <mru-stack> #:q '(#t #f)) #f)) => '(#t))

;;; if mru-remove allows multiple eq? elements then all other procs have
;;; to do the same for consistency.
(check (q (mru-recall (make <mru-stack> #:q '(a a b b c c)) 'a)) => '(a a b b c c))
(check (q (mru-recall (make <mru-stack> #:q '(a a b b c c)) 'c)) => '(c a a b b c))
(check (q (mru-recall (make <mru-stack> #:q '(#t #f)) #f)) => '(#f #t))

(check (equal? mru-set mru-recall) => #t)

;;; mru-ref :: mru -> #f | value, mru cannot reliably contain #f
(check (mru-ref s 0) => #f)
(check (mru-ref s 1) => #f)
(check (mru-ref (mru-add s 'a) 0) => 'a)
(check (mru-ref (mru-add s 'a) 3) => #f)

;;; mru->list :: mru -> list
(check (mru->list s) => '())

;;; mru-empty? :: mru -> bool
(check (mru-empty? s) => #t)
(check (mru-empty? (mru-add s '())) => #f)

;;; mru-contains? :: mru -> bool | non-negative integer
(check (mru-contains? s 'a) => #f)
(check (mru-contains? (mru-add s '()) '()) => 0)

(check (circular-list->list (circular-list 'a 'c 'd 'c)) => '(a c d c))

;;; mru-next and mru-prev always return a valid mru
(check (q (mru-next s 3)) => '())
(check (q (mru-next s -3)) => '())

(check (q (mru-prev s 4)) => '())
(check (q (mru-next s -4)) => '())

(check-exit)
