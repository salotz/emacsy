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
            mru-add!
            mru-remove!
            mru-recall!
            mru-set!
            mru-ref
            mru-empty?
            mru-contains?
            mru-next!
            mru-prev!
            mru-list))

;;; mru-stack used these from Q: make-q q-push! q-remove! q-empty?
;;; let's define those first

;;; first in last out
(define make-q list)

;;; reverse the order of args in cons
(define q-push xcons)

;;; remove all occurences of item
(define (q-remove q item)
  (filter (compose not (cut equal? <> item)) q))

(define q-empty? null-list?)

;;; onwards to mru part of the puzzle
(define-class <mru-stack> ()
  (queue #:accessor q #:init-thunk (lambda () (make-q)))
  (index #:accessor index #:init-value 0))

(define-method (write (obj <mru-stack>) port)
  (format port "<mru-stack ~a>" (mru-list obj)))

(define-method (mru-add! (s <mru-stack>) x)
  (set! (q s) (q-push (q s) x))
  (q s))

(define-method (mru-remove! (s <mru-stack>) item)
  (let ((old-item (mru-ref s)))
    (set! (q s) (q-remove (q s) item))
    (unless (eq? old-item item)
      (mru-set! s old-item))))

;;; note: easily the most important proc
(define-method (mru-recall! (s <mru-stack>) item)
  (set! (index s) 0)
  (set! (q s) (q-push (q-remove (q s) item) item))
  (mru-list s))

(define-method (mru-set! (s <mru-stack>) x)
  ;; Should this add the buffer if it's not already there? No.
  (if (mru-empty? s)
      #f
      (let ((i (member-ref x (mru-list s))))
        (if i
            (begin (set! (index s) i)
                   #t)
            (begin (mru-next! s)
                   #f)))))

;;.
(define-method (mru-ref (s <mru-stack>))
  (and (not (mru-empty? s))
       (list-ref (mru-list s) (index s))))

;;.
(define-method (mru-list (s <mru-stack>))
  (q s))

;;.
(define-method (mru-empty? (s <mru-stack>))
  (q-empty? (q s)))

;;.
(define-method (mru-contains? (s <mru-stack>) x)
  (memq x (mru-list s)))

;; The order of the elements may not change yet the index may be moved
;; around.
(define-method* (mru-next! (s <mru-stack>) #:optional (count 1))
  (unless (mru-empty?  s)
   (set! (index s)
         (modulo (+ (index s) count)
                 (length (mru-list s))))
   (mru-ref s)))

(define-method* (mru-prev! (s <mru-stack>) #:optional (count 1))
  (mru-next! s (- count)))
