;; Copyright (C) 2019  Amar Singh

;; Circular --- Desc

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
(define-module (emacsy circular)
  #:use-module ((ice-9 binary-ports) #:select (eof-object))
  #:export (
            partial
            rpartial
            pam
            pam!
            circular-list-null?
            circular-last-pair
            shader
            generator
            curry
            circular-list
            clist
            cnil
            ccons!
            cappend!
            circular-append!
            circular-append
            hook->list
            delq1
            circular-list->list
            ))

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

(define pam!
  (lambda (fns val)
      "Apply a list of functions FNS to a value VAL to produce a series of effects."
      (unless (null? fns)
          (begin ((car fns) val)
                (pam! (cdr fns) val)))))

(define circular-list-null?
  (lambda (clst)
    (and (eq? (car clst) clst)
         (eq? (cdr clst) clst))))

;;; FIXME: circular-last-pair is not correct, if you cons (regular cons)
;;; something to a circular list then it would diverge. The ideal choice
;;; would be to start building a list of encountered items by cdring
;;; down a list. If at some point the built list becomes equal to the
;;; current list then we have reached the end.
(define circular-last-pair
  (lambda (lst)
    (define c-last-pair
      (lambda (lst start)
        (if (eq? (cdr lst) start) lst
            (c-last-pair (cdr lst) start))))
    (c-last-pair lst lst)))

(define shader
  (lambda (in out reader-empty? proc reader-next reader writer)
    (if (reader-empty? in) out
        (shader (reader-next in)
                (writer (proc (reader in)) out) reader-empty? proc reader-next reader writer))))

(define (generator)
  (let ((args '()))
    (case-lambda
      ;; produce
      (() (if (null? args) (eof-object)
              (let ((res (car args)))
                (set! args (cdr args))
                res)))
      ;; consume
      ((magic) args)
      ((this) generator)
      ((a) (set! args (cons a args))))))

(define curry
  (lambda (proc . args)
    (if (zero? (length args))
        (lambda ()
          (apply proc args))
        (lambda rest
          (curry proc (append args rest))))))

(define copy
  (lambda (in out)
    (shader in out null? identity cdr car cons)))

(define ccopy
  (lambda (current seen)
    (if (null? current) seen
        (if (eq? current seen)
            seen
            (ccopy (cdr current) (cons (car current) seen))))))

(define clast-pair
  (lambda (clst)

    '()))

(define circular-list
  (case-lambda
    ((elt . elts) (let ((lst (cons elt elts)))
               (set-cdr! (last-pair lst) lst)
               lst))
    (() (let ((cl (circular-list '())))
          (set-car! cl cl)
          cl))))
;;; convenient alias
(define clist circular-list)

(define (circular-list->list q)
  (define (clst->list start q)
    (if (eq? start (car q))
        (list)
        (cons (car q) (clst->list start (cdr q)))))
  (cons (car q) (clst->list (car q) (cdr q))))

(define cnil (clist))

(define (ccons! elt clst)
  (if (circular-list-null? clst)
      (begin (set-car! clst elt)
        clst)
      (let ((end (circular-last-pair clst)))
        (set-cdr! end (cons elt clst))
        clst)))

(define (cappend! clst1 clst2 end)
  (if (eq? end clst1)
      (cons (car clst1) clst2)
      (cons (car clst1)
            (cappend! (cdr clst1) clst2 end))))

(define (lccons! elt clst)
  (if (circular-list-null? clst) (ccons! elt clst)
      (let ((end (circular-last-pair clst)))
        (set-cdr! end (cons elt clst))
        (cdr end))))

(define* (circular-append! clst1 clst2 #:optional order?)
  "Appends the elements of clist1 to clist2 destructively."
  (define (cappend clst1 clst2 end)
    (if (eq? end clst1)
        (cons (car clst1) clst2)
        (cons (car clst1)
              (cappend (cdr clst1) clst2 end))))
  (let ((ending-pair (circular-last-pair clst2)))
    (set-cdr! ending-pair
              (cappend clst1 clst2 (circular-last-pair clst1)))
    (if order? ending-pair
        clst2)))

(define* (circular-append clst1 clst2 #:optional order?)
  (cdr (circular-append! clst1
                         (cdr (circular-append! clst2 (clist))))))

(define (circular-list-copy clst)
  (cdr (circular-append! clst (clist))))

;;; hooks are from guile. unless list->hook is added to guile this
;;; should probably go to utilities.
(define (list->hook lst)
  "Convert the procedure list of LST to a hook."
  (define (adder h lst arity)
    (if (null? lst) h
        (let ((proc (car lst)))
          (if (procedure? proc)
              (if (equal? arity (car (procedure-minimum-arity proc)))
                  (begin (add-hook! h proc #t)
                         (adder h (cdr lst) arity))
                  (error (format #f "In procedure list->hook: Wrong number of arguments to ~s, expected arity ~s" proc arity)))
              (error (format #f "In procedure list->hook: Wrong type argument ~s, expecting a procedure." proc))))))
  (if (null? lst) (make-hook)
      (let ((proc (car lst)))
        (if (procedure? proc)
            (let* ((arity (car (procedure-minimum-arity proc)))
                   (hook (make-hook arity)))
              (adder hook lst arity))
            (error (format #f "In procedure list->hook: Wrong type argument: ~s in list, expected a procedure." proc))))))

(define (delq1 item lst)
  (if (null? lst) lst
      (let ((elt (car lst)))
        (if (eq? item elt)
            (cdr lst)
            (cons elt (delq1 item (cdr lst)))))))
