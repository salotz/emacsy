;;; \subsection*{File Layout}
;;;
;;;
;;; <file:buffer.scm>=
;;; \subsection{Legal Stuff}
;;;
;;; <+ Copyright>=
;;; Copyright (C) 2012, 2013 Shane Celis <shane.celis@gmail.com>
;;; <+ License>=
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
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 q)
  #:use-module (ice-9 gap-buffer)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26)
  #:use-module (string completion)
  #:use-module (oop goops)
  #:use-module (emacsy util)
  #:use-module (emacsy mru-stack)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy event)
  #:use-module (emacsy keymap)
  #:use-module (emacsy command)
  #:use-module (emacsy klecl)
  #:use-module (emacsy mode)
  #:use-module (rnrs base))
;;; A convenience macro to work with a given buffer.
;;;
;;;
;;; <buffer:macro>=
(define-syntax-public with-buffer
  (syntax-rules ()
    ((with-buffer buffer e ...)
     (let ((old-buffer (current-buffer)))
       (in-out-guard
         (lambda () (set-buffer! buffer))
         (lambda () e ...)
         (lambda () (set-buffer! old-buffer)))))))
;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-
;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-
;;; \section{Buffer Module}
;;;
;;; \epigraph{And when you gaze long into an abyss the abyss also gazes into you.}{Beyond Good and Evil \\Friedrich Nietzsche}
;;; \todo[inline]{This should be moved out of the KLECL chapter.}
;;;
;;; A buffer in Emacs represents text, including its mode, local
;;; variables, etc.  A Emacsy buffer is not necessarily text.  It can be
;;; extended to hold whatever the host application is interested in.
;;; Emacs' concepts of buffer, window, and mode are directly analogous to
;;; the model, view, and controller respectively---the MVC pattern.
;;;
;;;
;;; <buffer:class>=
(define-class-public <buffer> ()
  (name #:init-keyword #:name)
  (keymap #:accessor local-keymap #:init-keyword #:keymap #:init-form (make-keymap))
  (locals #:accessor local-variables #:init-form '())
  (buffer-modified? #:accessor buffer-modified? #:init-value #f)
  (buffer-modified-tick #:accessor buffer-modified-tick #:init-value 0)
  (buffer-enter-hook #:accessor buffer-enter-hook #:init-form (make-hook 0))
  (buffer-exit-hook #:accessor buffer-exit-hook #:init-form (make-hook 0))
  (buffer-modes #:accessor buffer-modes #:init-form '()))
(export local-keymap local-variables buffer-enter-hook buffer-exit-hook before-buffer-change-hook after-buffer-change-hook after-change-hook before-change-hook buffer-modified-tick buffer-modes)
;;; \subsection{Text Buffer}
;;;
;;; Our minibuffer and messages buffer require a text buffer.
;;;
;;;
;;; <buffer:class>=
(define-class-public <text-buffer> (<buffer>)
  (gap-buffer #:accessor gap-buffer #:init-form (make-gap-buffer "")))
(export gap-buffer)
;;; <buffer:state>=
(define-variable before-buffer-change-hook (make-hook 1) "This hook is called prior to the buffer being changed with one argument, the buffer.")
(define-variable after-buffer-change-hook (make-hook 1) "This hook is called after to the buffer has changed with one argument, the buffer.")
;;; \subsection{Buffer List}
;;; The buffer module also keeps track of the live buffers and the current
;;; one.
;;;
;;; \input{_mru-stack}
;;;
;;;
;;; <buffer:state>=
(define-public buffer-stack (make <mru-stack>))
(define-public last-buffer #f)
;;; <buffer:state>=
(define-public aux-buffer #f)
;;; Buffer's have a name, and there is always a current buffer or it's
;;; false.  Note that methods do not work as easily with optional
;;; arguments.  It seems best to define each method with a different
;;; number of arguments as shown below.
;;;
;;;
;;;
;;; <buffer:procedure>=
(define-method-public (buffer-name)
  (buffer-name (current-buffer)))

(define-method-public (buffer-name (buffer <buffer>))
  (slot-ref buffer 'name))

(define-method-public (set-buffer-name! name)
  (set-buffer-name! name (current-buffer)))

(define-method-public (set-buffer-name! name (buffer <buffer>))
  (slot-set! buffer 'name name))

(define-method-public (buffer-modified?)
  (buffer-modified? (current-buffer)))

(define-method-public (buffer-modified-tick)
  (buffer-modified-tick (current-buffer)))

(define-method (write (obj <buffer>) port)
  (write (string-concatenate (list "#<buffer '" (buffer-name obj) "'>")) port))
;;; \subsection{Emacs Compatibility}
;;;
;;;
;;; <buffer:procedure>=
(define-public (current-local-map)
  (local-keymap (current-buffer)))

(define-public (use-local-map keymap)
  (set! (local-keymap (current-buffer)) keymap))
;;; <buffer:procedure>=
(define-public (buffer-list)
  (mru-list buffer-stack))
;;; <buffer:procedure>=
(define-public (current-buffer)
  ;; Perhaps instead of returning #f for no buffer there should be an
  ;; immutable void-buffer class.
  (or aux-buffer
      (mru-ref buffer-stack)))
;;; <buffer:procedure>=
(define-public (add-buffer! buffer)
  (mru-add! buffer-stack buffer))
;;; <buffer:procedure>=
(define-public (remove-buffer! buffer)
  (mru-remove! buffer-stack buffer))

(define-interactive (next-buffer #:optional (incr 1))
  (mru-next! buffer-stack incr)
  (switch-to-buffer (mru-ref buffer-stack)))

(define-interactive (prev-buffer #:optional (incr 1))
  (next-buffer (- incr)))

(define-public (set-buffer! buffer)
  ;;(emacsy-log-debug "set-buffer! to ~a" buffer)
  (if (mru-set! buffer-stack buffer)
      (set! aux-buffer #f)
      (set! aux-buffer buffer)))

(define-interactive (kill-buffer #:optional (buffer (current-buffer)))
  (remove-buffer! buffer))

(define-interactive (other-buffer #:optional (count 1))
  (next-buffer count))
;;; This is our primitive procedure for switching buffers.  It does not
;;; handle any user interaction.
;;;
;;;
;;; <buffer:procedure>=
(define (primitive-switch-to-buffer buffer)
  (emacsy-log-debug "Running exit hook for ~a" (current-buffer))
  (run-hook (buffer-exit-hook (current-buffer)))
  (set! last-buffer (current-buffer))
  (if (mru-contains? buffer-stack buffer)
      (begin
        (emacsy-log-debug "Recall buffer ~a" buffer)
        (mru-recall! buffer-stack buffer)
        (set! aux-buffer #f))
      (begin
        (emacsy-log-debug "Set buffer to ~a" buffer)
        (set-buffer! buffer)))
  (emacsy-log-debug "Running enter hook for ~a" (current-buffer))
  (run-hook (buffer-enter-hook (current-buffer)))
  (current-buffer))

(define-public switch-to-buffer primitive-switch-to-buffer)
;;; \subsubsection{Local Variables}
;;;
;;;
;;; <buffer:procedure>=
(define (local-var-ref symbol)
  (let ((result (assq symbol (local-variables (current-buffer)))))
    (if (pair? result)
     (cdr result)
     ;(variable-ref (make-undefined-variable))
     (throw 'no-such-local-variable symbol))))

;; If buffers were in their own modules I could dynamically add variables
;; to their namespace.  Interesting idea.

(define (local-var-set! symbol value)
  (slot-set! (current-buffer)
             'locals
             (assq-set! (local-variables (current-buffer)) symbol value)))

(define-public local-var
               (make-procedure-with-setter local-var-ref local-var-set!))
;;; <buffer:procedure>=
(define-method-public (buffer-string (buffer <text-buffer>))
  (gb->string (gap-buffer buffer)))
;;; \subsubsection{Point}
;;; Now, let's implement the point procedures that control where the
;;; insertion point is within the buffer.
;;;
;;;
;;; <buffer:procedure>=
(define-method-public (point)
  (point (current-buffer)))

(define-method-public (point (buffer <text-buffer>))
  (gb-point (gap-buffer buffer)))

(define-method-public (point-min)
  (point-min (current-buffer)))

(define-method-public (point-min (buffer <text-buffer>))
  (gb-point-min (gap-buffer buffer)))

(define-method-public (point-max)
  (point-max (current-buffer)))

(define-method-public (point-max (buffer <text-buffer>))
  (gb-point-max (gap-buffer buffer)))
;;; Move the point around.  It's very tempting to change the name from
;;; [[goto-char]] to [[goto-point!]] because [[goto-char]] is misleading.
;;; You don't actually go to a character, you go to a place between
;;; characters, a point.  \todo{Change goto-char to goto-point!}
;;;
;;;
;;; <buffer:procedure>=
(define-method-public (goto-char point)
  (goto-char point (current-buffer)))

(define-method-public (goto-char point (buffer <text-buffer>))
  (gb-goto-char (gap-buffer buffer) point))
;(define goto-point! goto-char)
;;; Let's add the procedures [[char-before]] and [[char-after]] to inspect
;;; the characters before and after a point.
;;;
;;; <buffer:procedure>=
;; XXX define-method needs to allow for the definition of optional arguments.
;; This is getting ridiculous.
(define-method-public (char-before)
  (char-before (point) (current-buffer)))

(define-method-public (char-before point)
  (char-before point (current-buffer)))

(define (gb-char-before gb point)
  ;; Avert thy eyes.
  (if (<= point (gb-point-min gb))
      #f
      (string-ref (gb->string gb) (- point 2))))

(define (gb-char-after gb point)
  ;; Avert thy eyes.
  (if (>= point (gb-point-max gb))
      #f
      (string-ref (gb->string gb) (- point 1))))

(define-method-public (char-before point (buffer <text-buffer>))
  (gb-char-before (gap-buffer buffer) point))

(define-method-public (char-after)
  (char-after (point) (current-buffer)))

(define-method-public (char-after point)
  (char-after point (current-buffer)))

(define-method-public (char-after point (buffer <text-buffer>))
  (gb-char-after (gap-buffer buffer) point))

;;; There is
;;; \href{http://gnuvola.org/software/guile/doc/Gap-Buffer.html}{documentation}
;;; which suggests that [[(ice-9 gap-buffer)]] module has some nice regex
;;; search features.  However, I can't find the implementation anywhere,
;;; so I implemented them pretty sloppily here.  They should work find for
;;; a minibuffer, but probably shouldn't be used for anything bigger.
;;;
;;;
;;; <buffer:procedure>=
(define*-public (gb-re-search-forward gb regex
                                      #:optional (bound #f) (no-error? #f) (repeat 1))
  ;; This could be done better in the gap-buffer.scm itself.
  (if (= repeat 0)
      (gb-point gb)
      (let* ((string (gb->string gb))
             (pt (gb-point gb))
             (match (regexp-exec regex string (- pt 1))))
        ;;(format #t "match ~a ~%" match)
        (if match
            (begin
              (gb-goto-char gb (+ 1 (match:end match 0)))
              (gb-re-search-forward gb regex bound no-error? (1- repeat)))
            (if no-error?
                #f
                (scm-error 'no-match 'gb-re-search-forward

                           "No match found for regex '~a' in ~s after point ~a" (list regex string pt) #f))))))

(define*-public (gb-re-search-backward gb regex
                                       #:optional (bound #f) (no-error? #f) (repeat 1))
  ;; This could be done better in the gap-buffer.scm itself.
  (if (= repeat 0)
      (gb-point gb)
      (let loop ((start-search 0)
                 (last-match-start #f))
       (let* ((string (gb->string gb))
              (pt (gb-point gb))
              (match (regexp-exec regex string start-search)))
         (define (my-error)
           (if no-error?
               #f
               (scm-error
                'no-match 'gb-re-search-forward
                "No match found for regex '~a' in ~s before point ~a"
                (list regex string pt) #f)))
         (define (finish)
           (if last-match-start
               (begin
                 (gb-goto-char gb (1+ last-match-start))
                 (gb-re-search-backward gb regex bound no-error? (1- repeat)))
               (my-error)))
         ;;(format #t "match ~a ~%" match)
         (if match
             (if (< (match:start match 0) (1- pt))
                 ;; continue searching
                 (loop (match:end match 0) (match:start match 0))
                 (finish))
             (finish))))))
;;; Some commands to move the point around and insert or delete characters.
;;;
;;;
;;; <buffer:procedure>=
(define-interactive (kill-line #:optional (n 1))
  (gb-delete-char! (gap-buffer (current-buffer)) (- (point-max) (point))))

(define-interactive (delete-backward-char #:optional (n 1))
  (gb-delete-char! (gap-buffer (current-buffer)) (- n)))

(define-interactive (forward-delete-char #:optional (n 1))
  (gb-delete-char! (gap-buffer (current-buffer)) n))

(define-interactive (forward-char #:optional (n 1))
  (goto-char (+ (point) n)))

(define forward-word-regex (make-regexp "\\s*\\w+\\b"))
(define backward-word-regex (make-regexp "\\b\\w+\\s*"))

;; XXX where is gb-re-search-forward defined?  It has documentation.
;; but no implementation?
;; http://gnuvola.org/software/guile/doc/Gap-Buffer.html
(define-interactive (forward-word #:optional (n 1))
  (gb-re-search-forward (gap-buffer (current-buffer)) forward-word-regex #f #t n))

(define-interactive (backward-word #:optional (n 1))
  (gb-re-search-backward (gap-buffer (current-buffer)) backward-word-regex #f #t n))

(define-interactive (move-beginning-of-line #:optional (n 1))
  ;(gb-beginning-of-line (gap-buffer (current-buffer)) n)
  (goto-char (point-min)))

(define-interactive (move-end-of-line #:optional (n 1))
  ;(gb-beginning-of-line (gap-buffer (current-buffer)) n)
  (goto-char (point-max)))

(define-interactive (backward-char #:optional (n 1))
  (forward-char (- n)))
;;; Finally, we can insert text.
;;;
;;;
;;; <buffer:procedure>=
(define*-public (insert #:rest args)
  (and (current-buffer)
   (if (null? args)
       0
       (let ((arg (car args)))
         (run-hook before-buffer-change-hook (current-buffer))
         (cond
          ((string? arg)
           (gb-insert-string! (gap-buffer (current-buffer)) arg))
          ((char? arg)
           (gb-insert-char! (gap-buffer (current-buffer)) arg))
          (else #f))
         (set! (buffer-modified? (current-buffer)) #t)
         (incr! (buffer-modified-tick (current-buffer)))
         (run-hook after-buffer-change-hook (current-buffer))))))
;;; <buffer:procedure>=
(define-interactive (self-insert-command #:optional (n 1))
  (if (< n 1)
      ;; We're done.
      #f
      (let* ((event this-command-event))
        ;; Do I have to do anything for shifted characters?
        (insert (command-char event)))))