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
