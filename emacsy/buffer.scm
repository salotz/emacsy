;;; Emacsy --- An embeddable Emacs-like library using GNU Guile
;;;
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; Copyright (C) 2019 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
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

(define-module (emacsy buffer)
  #:use-module (ice-9 format)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy mru-stack)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (emacsy mode)
  #:export (<buffer>
            ;; accessors
            buffer-enter-hook
            buffer-exit-hook
            buffer-file-name
            buffer-keymap
            buffer-kill-hook
            buffer-modes
            buffer-modified-tick
            buffer-modified?
            buffer-name
            buffer-variables
            ;; variables
            ;; procs
            next-buffer
            prev-buffer
            buffer-previous!
            buffer-next!
            other-buffer!
            with-buffer
            save-excursion
            buffer-stack
            buffer-name
            buffer-name
            set-buffer-name!
            buffer-modified?
            buffer-modified-tick
            current-local-map
            use-local-map
            buffer-list
            current-buffer
            add-buffer!
            remove-buffer!
            set-buffer!
            switch-to-buffer
            local-var
            emacsy-mode-line))

;;; Commentary:

;;; buffer uses form mru-stack: <mru-stack> mru-list mru-ref mru-add!
;;; mru-remove! mru-next! mru-set! mru-recall! mru-contains?

;; @node Buffer
;; @section Buffer

;; @quotation
;; And when you gaze long into an abyss the abyss also gazes into you.
;; @author Beyond Good and Evil, Friedrich Nietzsche
;; @end quotation

;; A buffer in Emacs represents text, including its mode, local
;; variables, etc.  A Emacsy buffer is not necessarily text.  It can be
;; extended to hold whatever the host application is interested in.
;; Emacs' concepts of buffer, window, and mode are directly analogous to
;; the model, view, and controller respectively---the MVC pattern.

;;; Code:

;; @defmac with-buffer @dots{}
;; A convenience macro to work with a given buffer.
;; @end defmac
(define-syntax with-buffer
  (syntax-rules ()
    ((with-buffer buffer e ...)
     (let ((old-buffer (current-buffer))
           (result *unspecified*))
       (in-out-guard
        (lambda () (set-buffer! buffer))
        (lambda () e ...)
        (lambda () (set-buffer! old-buffer)))))))

;; @defmac save-excursion @dots{}
;; A convenience macro to do some work
;; @end defmac
(define-syntax save-excursion
  (syntax-rules ()
    ((save-excursion body ...)
     (let ((old-buffer (current-buffer))
           (old-point (point)))
       (in-out-guard
        (lambda _ #t)
        (lambda _ body ...)
        (lambda _
          (set-buffer! old-buffer)
          (goto-char old-point)))))))

;;; buffers are used by text.scm, introspection.scm, minibuffer.scm, emacsy.scm, core.scm
(define-class <buffer> ()
  (name #:accessor buffer-name #:init-keyword #:name #:init-value "")
  (file-name #:accessor buffer-file-name #:init-keyword #:file-name #:init-form #f)
  (keymap #:accessor buffer-keymap #:init-keyword #:keymap #:init-form (make-keymap))
  (variables #:accessor buffer-variables #:init-keyword #:variables #:init-form '())
  (modified? #:accessor buffer-modified? #:init-keyword #:modified #:init-value #f)
  (modified-tick #:accessor buffer-modified-tick #:init-keyword #:modified-tick #:init-value 0)
  (enter-hook #:accessor buffer-enter-hook #:init-keyword #:enter-hook #:init-form (make-hook 0))
  (exit-hook #:accessor buffer-exit-hook #:init-keyword #:exit-hook #:init-form (make-hook 0))
  (kill-hook #:accessor buffer-kill-hook #:init-keyword #:kill-hook #:init-form (make-hook 0))
  (modes #:accessor buffer-modes #:init-keyword #:modes #:init-form '()))


(define %void-buffer (make <buffer>))

(define-method (write (obj <buffer>) port)
  (format port "#<buffer ~a>" (buffer-name obj)))
;; @c @node
;; @subsection Emacs Compatibility

;;;;;; These are all just mru-stack, here called buffer-stack ops, the
;;;;;; fuck, why am i redoing this, it's pretty much the same damn
;;;;;; thing.
;;; :: buffer-stack -> list
(define (buffer-list buffer-stack)
  (mru->list buffer-stack))

;;; buffer-stack ops :: buffer-stack [+ ARGS] -> buffer-stack
(define (current-buffer buffer-stack)
  (or (mru-ref buffer-stack)
      %void-buffer))

(define* (buffer-ref buffer-stack #:optional (ref 0))
  (mru-ref buffer-stack ref))

(define (add-buffer buffer-stack buffer)
  (mru-add buffer-stack buffer))

(define (remove-buffer buffer-stack buffer)
  (mru-remove buffer-stack buffer))

(define* (buffer-previous buffer-stack #:optional (incr 1))
  (mru-next buffer-stack incr))

(define* (buffer-next buffer-stack #:optional (incr 1))
  (buffer-previous buffer-stack (- incr)))

(define (buffer-set buffer-stack buffer)
  ;; (emacsy-log-debug "set-buffer! to ~a" buffer)
  (if (mru-contains? buffer-stack buffer)
      (mru-recall buffer-stack buffer)))

(define* (other-buffer buffer-stack #:optional (incr 1))
  (current-buffer (mru-recall buffer-stack
                              (list-ref (buffer-list) incr))))

;;; end
;;;;;; end

;;; This is our primitive procedure for switching buffers.  It does not
;;; handle any user interaction.
(define* (primitive-switch-to-buffer buffer-stack buffer #:optional (recall? #t))
  (emacsy-log-debug "Running exit hook for ~a" (current-buffer buffer-stack))
  (run-hook (buffer-exit-hook (current-buffer buffer-stack)))
  (if recall?
      (begin
        (emacsy-log-debug "Recall buffer ~a" buffer)
        (set-buffer! buffer))
      (begin
        (emacsy-log-debug "Add buffer ~a" buffer)
        (add-buffer! buffer)))
  (emacsy-log-debug "Running enter hook for ~a" (current-buffer buffer-stack))
  (run-hook (buffer-enter-hook (current-buffer buffer-stack)))
  (current-buffer buffer-stack))

(define switch-to-buffer primitive-switch-to-buffer)

;; If buffers were in their own modules I could dynamically add variables
;; to their namespace.  Interesting idea.

;;; interactive fns :: A [+ ARGS] -> unspecified
(define-interactive (next-buffer buffer-stack #:optional (incr 1))
  (switch-to-buffer (mru-ref (buffer-next buffer-stack incr))))

(define-interactive (prev-buffer buffer-stack #:optional (incr 1))
  (next-buffer buffer-stack (- incr)))

;; This is scary, we will override it when we have <text-buffer>.
(define-interactive (kill-buffer buffer-stack #:optional buffer)
  (remove-buffer buffer-stack buffer))

(define-interactive (other-buffer buffer-stack #:optional (count 1))
  (switch-to-buffer (other-buffer! buffer-stack count))
  #t)

;; method
(define-method* (emacsy-mode-line buffer)
  (format #f "-:~a- ~a    (~{~a~^ ~})"
          (if (buffer-modified? buffer) "**"
              "--")
          (buffer-name buffer)
          (map mode-name (buffer-modes buffer))))
;;; end
