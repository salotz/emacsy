;; Copyright (C) 2019  Amar Singh

;; CSP --- comm seq process

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

(define-module (emacsy csp)
  #:use-module (emacsy monad)
  #:use-module ((srfi srfi-1) #:select (find))
  #:use-module (emacsy circular))

;;; comm sequential processes
(define-public vm
  (letrec ((s1 (lambda (event)
                 (if (eq? event 'reset) vm
                     (if (eq? event 'coin)
                         (begin (pk "take a choc!")
                                vm)
                         s1))))
           (s2 (lambda (event)
                 (if (eq? event 'reset) vm
                     (if (eq? event 'choc)
                         (begin (pk "take a choc!")
                                vm)
                         s2)))))
    (lambda (event)
      (cond ((eq? event 'choc) s1)
            ((eq? event 'coin) s2)))))

;;; this second version is imperative because of the use of generator
(define-public events (generator))

(define-public vms
  (letrec ((finder (lambda (e)
                  (let ((item (events)))
                    (if (eof-object? item) #f
                        (if (eq? item e) #t
                            (finder e)))))))
    (lambda (event)
      (cond
       ((eq? event 'choc) (finder 'coin))
       ((eq? event 'coin) (finder 'choc))))))

;;; tony made a nice little language to make vending machines
;;; ah, i see it's a lot more work and very little gain to implement such a thing.

;;; traces
(define-public (tracevm)
  (letrec* ((choc (cons 'choc
                        (lambda (event)
                          (if (eq? event 'coin)
                              (action! 'choc)
                              (trace choc)))))
            (coin (cons 'coin
                        (lambda (event)
                          (if (eq? event 'choc)
                              (action! 'coin)
                              (trace coin)))))
            (home (cons 'home
                        (lambda (event)
                          (trace (find (compose (partial eq? event) car) states)))))
            (states (list coin choc home (cons 'debug trace)))
            (trace (let ((events '()))
                     (lambda (state)
                       (if (eq? (car state) 'debug) events
                           (begin (set! events (cons (car state) events))
                             (cdr state))))))
            (action! (lambda (e)
                       "give choc and reset to original state."
                       (begin (pk "take a choc!") (trace (cons e (cdr home)))))))
    (cdr home)))
;;; the above tracevm is not satisfactory, i am setting traces manually.

;;; it appears that not having tony's small language for defining
;;; vending machines is too much work.
(define-public (uvm e)
  (define states '(s0 s1 s2))
  (let ((res (find (partial eq? e) states)))
    (if res
        res
        'not-found)))

(define-public (make-vm states)
  (let ((states states))
    (lambda (e)
      (let ((state (find (compose (partial eq? e) car) states)))
        (if state
            (make-vm ((cadr state)))
            'not-found)))))

(define-public mvm-states
  (let ((args '()))
    (define s0 (list 's0 (lambda () (pk (car s0)) (list s1))))
    (define s1 (list 's1 (lambda () (pk (car s1)) (list s2))))
    (define s2 (list 's2 (lambda () (pk (car s2)) (list s0))))
    (list s0 s1 s2)))

;;; usage (define my-vm (make-vm mvm-states))
;;; states ::= list of state
;;; state ::= pair of symbol and null arity proc that produces a state
