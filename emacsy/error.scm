;; Emacsy --- An embeddable Emacs-like library using GNU Guile

;; Copyright (C) 2019 Amar Singh<nly@disroot.org>

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

;;; Commentary: Do 1. Throw useful errors from emacsy modules,
;;; originating from Emacsy api's or Emacsy's context. 2. But wait,
;;; don't crash my program, instead let me use the error in my
;;; program. Hence, let's have an Error reporting API. Use this to get
;;; Emacsy errors and use them in your application.

(define-module (emacsy error)
  #:use-module (oop goops)
  ;; just as a heart cannot live without a body, or a brain without the
  ;; heart, and so on, programs cannot exist without each other, but
  ;; together they can.
  #:use-module (emacsy mru-stack)
  #:use-module (emacsy util)
  #:use-module (srfi srfi-1)
  #:export (
            ;; class
            <error>
            ;; accessors
            error-message
            error-continuation
            error-from-list
            error-to-list
            ;; to and from list
            error->list
            list->error
            ))
;;; helpers, probably should be moved to emacsy util.
(define partial
  (lambda (proc . args)
    (lambda rest
      (apply proc (append args rest)))))

(define rpartial
  (lambda (proc . args)
    (lambda rest
      (apply proc (reverse (append args rest))))))

(define pam
  (lambda (fns val)
      "Apply a list of functions FNS to a value VAL to produce a list of
values"
      (if (null? fns) '()
          (cons ((car fns) val)
                (pam (cdr fns) val)))))

(define (error->list error)
  (let* ((accessor-symbols (map (compose (partial symbol-append 'error-)
                                  slot-definition-name)
                         (class-slots <error>)))
         (accessors (map (partial module-ref (current-module)) accessor-symbols)))
    (zip accessor-symbols (pam accessors error))))

(define (list->error lst)
  (let* ((accessor-symbols (map car lst))
         (accessors (map (lambda (x)
                           (lambda (y)
                             (assoc-ref y x)))
                         accessor-symbols)))
    (make <error>
      #:message (car (assoc-ref lst 'error-message))
      #:continuation (car (assoc-ref lst 'error-continuation))
      #:to-list error->list
      #:from-list list->error)))

;;; let's do error reporting for mru, i like how emacs displays errors
;;; inside itself. Applications using Emacsy might want to do that,
;;; catching the errors and re-directing them to their system of
;;; displaying errors.
;;; an error is an alist ((error-message "")(error-continuation K) ...)
(define-class <error> ()
  (message #:accessor error-message #:init-keyword #:message #:init-value "")
  (continuation #:accessor error-continuation #:init-keyword #:continuation #:init-value (lambda () '()))
  (to-list #:accessor error-to-list #:init-keyword #:to-list #:init-value error->list)
  (from-list #:accessor error-from-list #:init-keyword #:from-list #:init-value list->error))
