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
  #:export (<mru-stack>
            ;; accessor
            mru-list
            ;; procs
            mru-add
            mru-remove
            mru-recall
            mru-set
            mru-ref
            mru-other
            mru-empty?
            mru-contains?
            mru-next
            mru-prev
            ))

;;; Return on Failure is an error
(define LIST-BEGIN 0)
(define INCR 1)

(define (delq1 item lst)
  (if (null? lst) lst
      (let ((elt (car lst)))
        (if (eq? item elt)
            (cdr lst)
            (cons elt (delq1 item (cdr lst)))))))

(define-class <mru-stack> ()
  (list #:accessor mru-list #:init-keyword #:list #:init-thunk (lambda () (list))))

(define-method (write (obj <mru-stack>) port)
  (format port "<mru-stack ~a>" (mru-list obj)))

(define-method (mru-empty? (s <mru-stack>))
  (null-list? (mru-list s)))

(define-method (mru-contains? (s <mru-stack>) x)
  (list-index (lambda (y) eq? y x) (mru-list s)))

(define-method (mru-add (s <mru-stack>) x)
  (make <mru-stack> #:list (xcons (mru-list s) x)))

(define-method (mru-remove (s <mru-stack>) item)
  (make <mru-stack> #:list (delq1 item (mru-list s))))

(define-method (mru-recall (s <mru-stack>) item)
  (if (mru-contains? s item)
      (mru-add (mru-remove s item) item)
      (error (format #f "In procedure mru-recall: ~s does not exist in ~s"
                          item s))))

(define-method (mru-set (s <mru-stack>) item)
  (if (mru-contains? s item)
      (mru-recall s item)
      s))

(define-method* (mru-ref (s <mru-stack>) #:optional (ref LIST-BEGIN))
  (if (mru-empty? s)
      (error (format #f "In procedure mru-ref: Empty Mru-stack ~s" s))
      (if (>= ref (length (mru-list s)))
          (error (format #f "In procedure mru-ref: Ref ~s not found in ~s of length ~s" ref s (length (mru-list s))))
          (list-ref (mru-list s) ref))))

(define-method* (mru-other (s <mru-stack>) #:optional (ref INCR))
  (mru-recall s (mru-ref s ref)))

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
        #:list (let ((proc (if (negative? count) reverse identity))
                  (count (if (negative? count) (- count) count)))
              (proc (circular-list->list (drop (apply circular-list
                                                      (proc (mru-list s)))
                                               count)))))))

(define-method* (mru-prev (s <mru-stack>) #:optional (count INCR))
  (mru-next s (- count)))
