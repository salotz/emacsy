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

;;; Return on Failure
(define ROF #f)
(define LIST-BEGIN 0)
(define INCR 1)

(define-class <mru-stack> ()
  (queue #:accessor q
         #:init-keyword #:q
         #:init-thunk (lambda () (list))))

(define-method (write (obj <mru-stack>) port)
  (format port "<mru-stack ~a>" (mru->list obj)))

;;; mru is quite effectful, this might become an issue. make composable
;;; alternatives to mru-X! -> mru-X
(define-method (mru-add (s <mru-stack>) x)
  (make <mru-stack> #:q (xcons (q s) x)))

(define-method (mru-remove (s <mru-stack>) item)
  (make <mru-stack> #:q (delq1! item (list-copy (q s)))))

;;; note: easily the most important proc
(define-method (mru-recall (s <mru-stack>) item)
  (mru-add (mru-remove s item) item))

(define-method (mru-set (s <mru-stack>) item)
  (if (mru-contains? buffer-stack buffer)
      (mru-recall s item)
      s))

(define-method* (mru-ref (s <mru-stack>) #:optional (ref LIST-BEGIN))
  (if (mru-empty? s) ROF
      (if (>= ref (length (mru->list s))) ROF
          (list-ref (mru->list s) ref))))

(define-method (mru->list (s <mru-stack>))
  (q s))

(define-method (->list (s <mru-stack>))
  (q s))

(define-method (mru-empty? (s <mru-stack>))
  (null-list? (q s)))

(define-method (mru-contains? (s <mru-stack>) x)
  (list-index (cut eq? <> x) (mru->list s)))

(define (circular-list->list q)
  (define (clst->list start q)
    (if (eq? start (car q))
        '()
        (cons (car q) (clst->list start (cdr q)))))
  (cons (car q) (clst->list (car q) (cdr q))))

;;; FIXME: performance can be gained by defining (encylce! lst) -> circular-list
(define-method* (mru-next (s <mru-stack>) #:optional (count INCR))
  (if (mru-empty? s) s
      (make <mru-stack>
        #:q (let ((proc (if (negative? count) reverse identity))
                  (count (if (negative? count) (- count) count)))
              (proc (circular-list->list (drop (apply circular-list
                                                      (proc (mru->list s)))
                                               count)))))))

(define-method* (mru-prev (s <mru-stack>) #:optional (count INCR))
  (mru-next s (- count)))
