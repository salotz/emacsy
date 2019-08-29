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
            buffer-to-list
            buffer-from-list
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
            buffer->list
            list->buffer
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
  (modes #:accessor buffer-modes #:init-keyword #:modes #:init-form '())
  ;; keep a copy of dna; not really
  (to-list #:accessor buffer-to-list #:init-keyword #:to-list #:init-form buffer->list)
  (from-list #:accessor buffer-from-list #:init-keyword #:from-list #:init-form list->buffer))

;;; code is data.

;;; Code is no longer data with #<buffer > #<record > and
;;; such.  many items in a buffer are not represenable as a literal,
;;; check if the object is a non-literal, then 1. somehow ask each
;;; object to supply a method to convert it to a list, this proc should
;;; be contained in the object itself maybe? OR 2. just use lists in the
;;; first place.

;;; add a proc to go from buffer -> list. List are extensible, <buffer>
;;; or any object is in-extensible or extensible at a bigger cost.
(define (buffer->list buffer)
  (list (list 'buffer-name (buffer-name buffer))
        (list 'buffer-file-name (buffer-file-name buffer))
        (list 'buffer-keymap ((keymap-to-list (buffer-keymap buffer))
                              (buffer-keymap buffer)))
        (list 'buffer-variables (buffer-variables buffer))
        (list 'buffer-modified? (buffer-modified? buffer))
        (list 'buffer-modified-tick (buffer-modified-tick buffer))
        (list 'buffer-enter-hook (buffer-enter-hook buffer))
        (list 'buffer-exit-hook (buffer-exit-hook buffer))
        (list 'buffer-kill-hook (buffer-kill-hook buffer))
        (list 'buffer-modes (buffer-modes buffer))
        ;; keep a copy of dna
        (list 'buffer-from-list list->buffer)
        (list 'buffer-to-list buffer->list)))

(define (list->buffer lst)
  (make <buffer>
    #:name (car (assoc-ref lst 'buffer-name))
    #:file-name (car (assoc-ref lst 'buffer-file-name))
    #:keymap (car (assoc-ref lst 'buffer-keymap))
    #:variables (car (assoc-ref lst 'buffer-variables))
    #:modified? (car (assoc-ref lst 'buffer-modified?))
    #:modified-tick (car (assoc-ref lst 'buffer-modified-tick))
    #:enter-hook (car (assoc-ref lst 'buffer-enter-hook))
    #:exit-hook (car (assoc-ref lst 'buffer-exit-hook))
    #:kill-hook (car (assoc-ref lst 'buffer-kill-hook))
    #:modes (car (assoc-ref lst 'buffer-modes))
    ;; a copy of dna
    #:to-list buffer->list
    #:from-list list->buffer))

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
  (mru-set buffer-stack buffer))

(define* (buffer-other buffer-stack #:optional (incr 1))
  (current-buffer (mru-recall buffer-stack
                              (list-ref (buffer-list buffer-stack) incr))))

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
  (switch-to-buffer buffer-stack (mru-ref (buffer-next buffer-stack incr))))

(define-interactive (prev-buffer buffer-stack #:optional (incr 1))
  (next-buffer buffer-stack (- incr)))

;; This is scary, we will override it when we have <text-buffer>.
(define-interactive (kill-buffer buffer-stack #:optional buffer)
  (remove-buffer buffer-stack buffer))

(define-interactive (other-buffer buffer-stack #:optional (count 1))
  (switch-to-buffer buffer-stack (buffer-other buffer-stack count))
  #t)

;; method
(define-method* (emacsy-mode-line buffer)
  (format #f "-:~a- ~a    (~{~a~^ ~})"
          (if (buffer-modified? buffer) "**"
              "--")
          (buffer-name buffer)
          (map mode-name (buffer-modes buffer))))
;;; end
