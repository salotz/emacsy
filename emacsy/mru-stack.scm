;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 by Amar Singh<nly@disroot.org>
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

;;; Commentary:

;;; FIXME: introduced a test regression, now test/minibuffer.scm is
;;; failing.

;; @node Mru-stack
;; @subsection Mru-stack

;; The buffers are kept in a most recently used stack that has the
;; following operators: add!, remove!, contains?, recall!, and list.

;;; Code:

(define-module (emacsy mru-stack)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (<mru-stack>
            mru-add
            mru-remove
            mru-recall
            mru-set
            mru-ref
            mru-empty?
            mru-contains?
            mru-next
            mru-prev
            mru->list))

;;; mru-stack used these from Q: make-q q-push! q-remove! q-empty?
;;; let's define those first

;;; first in last out
(define make-q list)

;;; reverse the order of args in cons
(define q-push xcons)

;;; remove all occurences of item
(define (q-remove q item)
  (delq item q))

(define q-empty? null-list?)

;;; onwards to mru part of the puzzle
(define-class <mru-stack> ()
  (queue #:accessor q
         #:init-keyword #:q
         #:init-thunk (lambda () (make-q))))

(define-method (write (obj <mru-stack>) port)
  (format port "<mru-stack ~a>" (mru->list obj)))

;;; mru is quite effectful, this might become an issue. make composable
;;; alternatives to mru-X! -> mru-X
(define-method (mru-add (s <mru-stack>) x)
  (make <mru-stack> #:q (q-push (q s) x)))

(define-method (mru-remove (s <mru-stack>) item)
  (make <mru-stack> #:q (q-remove (q s) item)))

;;; note: easily the most important proc
(define-method (mru-recall (s <mru-stack>) item)
   (mru-add (mru-remove s item) item))

(define mru-set mru-recall)

;;.
(define-method* (mru-ref (s <mru-stack>) #:optional (ref 1))
  (unless (mru-empty? s)
       (list-ref (mru->list s) ref)))

;;.
(define-method (mru->list (s <mru-stack>))
  (q s))

;;.
(define-method (mru-empty? (s <mru-stack>))
  (q-empty? (q s)))

;;.
(define-method (mru-contains? (s <mru-stack>) x)
  (list-index (cut eq? <> x) (mru->list s)))

(define-public (circular-list->list q)
  (define (clst->list* start q)
    (if (eq? start (car q))
        '()
        (cons (car q) (clst->list* start (cdr q)))))
  (cons (car q) (clst->list* (car q) (cdr q))))

;;; FIXME: performance can be gained by defining (encylce! lst) -> circular-list
(define-method* (mru-next (s <mru-stack>) #:optional (count 1))
  (unless (mru-empty? s)
    (make <mru-stack>
      #:q (circular-list->list (drop (apply circular-list
                                            (mru->list s))
                                     count)))))

(define-method* (mru-prev (s <mru-stack>) #:optional (count 1))
  (make <mru-stack>
    #:q (reverse (mru->list (mru-next (make <mru-stack>
                                        #:q (reverse (mru->list s))) count)))))
