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
  #:use-module ((ice-9 session) #:select (procedure-arguments))
  #:use-module (oop goops)
  #:use-module (emacsy circular)
  #:use-module (emacsy monad)
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
            ;; syntax
            with-buffer
            save-excursion
            ;; variables
            ;; procs
            next-buffer
            prev-buffer
            kill-buffer
            buffer-ref
            buffer-previous
            buffer-next
            buffer-other
            buffer-set
            other-buffer
            with-buffer
            save-excursion
            buffer-list
            current-buffer
            add-buffer
            remove-buffer
            set-buffer
            switch-to-buffer
            buffer->list
            list->buffer
            list->hook
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
  "FIXME: redo this (buffer->list and list->buffer) with pattern
matching."
  (list (list 'buffer-name (buffer-name buffer))
        (list 'buffer-file-name (buffer-file-name buffer))
        (list 'buffer-keymap ((keymap-to-list (buffer-keymap buffer))
                              (buffer-keymap buffer)))
        (list 'buffer-variables (buffer-variables buffer))
        (list 'buffer-modified? (buffer-modified? buffer))
        (list 'buffer-modified-tick (buffer-modified-tick buffer))
        (list 'buffer-enter-hook (hook->list (buffer-enter-hook buffer)))
        (list 'buffer-exit-hook (hook->list (buffer-exit-hook buffer)))
        (list 'buffer-kill-hook (hook->list (buffer-kill-hook buffer)))
        (list 'buffer-modes (buffer-modes buffer))
        ;; keep a copy of dna
        (list 'buffer-from-list list->buffer)
        (list 'buffer-to-list buffer->list)))

(define (list->buffer lst)
  (make <buffer>
    #:name (car (assoc-ref lst 'buffer-name))
    #:file-name (car (assoc-ref lst 'buffer-file-name))
    #:keymap (let* ((keymap-lst (car (assoc-ref lst 'buffer-keymap)))
                    ;; dna in action
                    (keymap-from-list (car (assoc-ref keymap-lst 'keymap-from-list))))
               (keymap-from-list keymap-lst))
    #:variables (car (assoc-ref lst 'buffer-variables))
    #:modified? (car (assoc-ref lst 'buffer-modified?))
    #:modified-tick (car (assoc-ref lst 'buffer-modified-tick))
    #:enter-hook (list->hook (car (assoc-ref lst 'buffer-enter-hook)))
    #:exit-hook (list->hook (car (assoc-ref lst 'buffer-exit-hook)))
    #:kill-hook (list->hook (car (assoc-ref lst 'buffer-kill-hook)))
    #:modes (car (assoc-ref lst 'buffer-modes))
    ;; a copy of dna
    #:to-list buffer->list
    #:from-list list->buffer))

(define-method (write (obj <buffer>) port)
  (format port "#<buffer ~a>" (buffer-name obj)))
;; @c @node
;; @subsection Emacs Compatibility

;;; FIXME: buffer-list, buffer-ref, etc. are just a wrapper for
;;; mru. This had been a problem before when errors related to mru would
;;; be very hard to diagnose. Do 1. Better error reporting, errors
;;; should originate from buffer api, not lower level procs. But before
;;; that 0. Finish the functional api.

;;; ok, have a monadic api, looks like this: (<< (buffer-stack (>>
;;; add-buffer (make <buffer> #:name "anon")))) 1. Clean it up, instead
;;; of requiring 3 wrapper procs, maybe we can do with just two? 1. >>
;;; to send and 2. << to return?
(define buffer-stack
  ;; stage the expressions to be evaluated in a buffer-stack monad.
  (partial monadic (make <mru-stack>)))

(define (msend proc arg)
  ;; wrapper for procs entering buffer-stack monad.
  (m (rpartial proc) arg))

;; get a return-value from a monad.
(define mreturn fire)

(export buffer-stack msend mreturn)

;;; now looks like:
;; > (mreturn
;;           (msend add-buffer buffer-1)
;;           (msend add-buffer buffer-2)
;;           more-stuff)

;; $11 = <mru-stack (#<buffer winner> #<buffer anon>)>

;;; :: mru-stack -> list
(define (buffer-list buffer-stack)
  (mru-list buffer-stack))

;;; buffer-stack ops :: buffer-stack [+ ARGS] -> buffer-stack
;;; mru-stack ops

;;; :: mru-stack -> buffer
(define (current-buffer buffer-stack)
  (mru-ref buffer-stack))

(export buffer-ref add-buffer remove-buffer buffer-previous buffer-next buffer-set buffer-other)

;;; :: mru-stack -> buffer
(define buffer-ref  mru-ref)

;;; :: mru-stack -> mru-stack
(define add-buffer mru-add)

;;; :: mru-stack -> mru-stack
(define remove-buffer mru-remove)

;;; :: mru-stack -> mru-stack
(define buffer-previous mru-next)

;;; :: mru-stack -> mru-stack
(define buffer-next mru-prev)

;;; :: mru-stack -> mru-stack
(define buffer-set mru-set)

;;; :: mru-stack -> mru-stack
(define* (buffer-other buffer-stack #:optional (incr 1))
  (mru-recall buffer-stack
              (buffer-ref buffer-stack incr)))

;;; end

;;; This is our primitive procedure for switching buffers.  It does not
;;; handle any user interaction.
(define* (primitive-switch-to-buffer buffer-stack buffer #:optional (recall? #t))
  ;; FIXME: Adding of new buffers is not satisfactory
  (emacsy-log-debug "Running exit hook for ~a" (current-buffer buffer-stack))
  (run-hook (buffer-exit-hook (current-buffer buffer-stack)))
  (pam! (list (compose run-hook buffer-enter-hook)
              (partial emacsy-log-debug "Running enter hook for ~a"))
        (current-buffer (if (and (mru-contains? buffer-stack buffer) recall?)
                            (begin (emacsy-log-debug "Recall buffer ~a" buffer)
                                   (buffer-set buffer-stack buffer))
                            (begin (emacsy-log-debug "Add buffer ~a" buffer)
                                   (add-buffer buffer-stack buffer))))))

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
