;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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
(use-modules (check)
             (emacsy mru-stack)
             (emacsy buffer)
             (emacsy command)
             (emacsy event)
             (emacsy keymap)
             (oop goops)
             (rnrs base))

(use-private-modules (emacsy buffer))

;;; <+ Test Preamble>=
(use-modules (check))
(use-modules (ice-9 pretty-print))
(define test-errors '())
(use-modules (emacsy buffer) (emacsy mru-stack) (oop goops))
(define buf (make <buffer>))

;;; accessor; get
(buffer-name buf) => ""
(buffer-file-name buf) => #f
(buffer-keymap buf) => #<keymap >
(buffer-variables buf) => ()
(buffer-modified? buf) => #f
(buffer-modified-tick buf) => 0
(buffer-enter-hook buf) => #<hook 0 1e41a20>
(buffer-exit-hook buf) => #<hook 0 1e41a10>
(buffer-kill-hook buf) => #<hook 0 1e41a00>
(buffer-modes buf) => ()

;;; accessor; set
(set! (buffer-name buf) "acdc") => "acdc"
(buffer-name buf) => "acdc"
(set! (buffer-file-name buf) "/tmp/test") => "/tmp/test"
(buffer-file-name buf) => "/tmp/test"
(set! (buffer-keymap buf) "animals") => "animals"
(buffer-keymap buf) => "animals"
(set! (buffer-variables buf) "animals") => "animals"
(buffer-variables buf) => "animals"
(set! (buffer-modified? buf) "animals") => "animals"
 (buffer-modified? buf) => "animals"
(set! (buffer-modified-tick buf) "animals") => "animals"
 (buffer-modified-tick buf) => "animals"
(set! (buffer-enter-hook buf) "animals") => "animals"
(buffer-enter-hook buf) => "animals"
(set! (buffer-exit-hook buf) "animals") => "animals"
(buffer-exit-hook buf) => "animals"
(set! (buffer-kill-hook buf) "animals") => "animals"
(buffer-kill-hook buf) => "animals"
(set! (buffer-modes buf) "animals") => "animals"
(buffer-modes buf) => "animals"

buf => #<buffer acdc>

(eq? (@@ (emacsy buffer) void-buffer) (current-buffer (make <mru-stack>))) => #t

(call-with-output-string (lambda (port) (write buf port))) => "#<buffer acdc>"

(buffer-list (make <mru-stack>)) => ()

;;; <buffer:test>=
(define b (make <buffer> #:name "*test-buffer*"))
(check (buffer-name b) => "*test-buffer*")
(check (object->string b) => "#<buffer *test-buffer*>")
(check (current-buffer) => void-buffer)
;;; <buffer:test>=
(add-buffer! b)
(check (buffer-name) => "*test-buffer*")
(remove-buffer! b)
(check (current-buffer) => void-buffer)

(add-buffer! b)
(warn 'buffer-list (buffer-list))
(define a (make <buffer> #:name "*a*"))
(add-buffer! a)
(check (current-buffer) => a)
(switch-to-buffer b)
(check (current-buffer) => b)
(switch-to-buffer a)
(check (current-buffer) => a)

;;; <+ Test Postscript>=
;(run-tests)
(check-report)
'(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))
