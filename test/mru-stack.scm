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
;;; this is testing for "order of elements may not change yet the index
;;; may be moved around", here it expects mru-ref -> b, but the first
;;; element is a.
;; (check (mru->list s) => '(a c b))
(set! s (mru-remove s 'c))
;; (check (mru->list s) => '(a b))
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
(check-exit)
