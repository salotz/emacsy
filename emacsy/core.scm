;;; \subsection*{File Layout}                                               
;;;                                                                         
;;;                                                                         
;;; <file:core.scm>=                                                        
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
(define-module (emacsy core)
  #:use-module (ice-9 format)
  #:use-module (ice-9 q)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 readline)
  #:use-module (oop goops)
  #:use-module (rnrs io ports)
  #:use-module (debugging assert)
  #:use-module (system repl error-handling)
  #:use-module (srfi srfi-1)  ;; take
  #:use-module (srfi srfi-11) ;; let-values
  #:use-module (srfi srfi-26) ;; cut cute

  #:use-module (emacsy util)
  #:use-module (emacsy self-doc)
  #:use-module (emacsy keymap)
  #:use-module (emacsy event)
  #:use-module (emacsy mode)
  #:use-module (emacsy buffer)
  #:use-module (emacsy command)
  #:use-module (emacsy block)
  #:use-module (emacsy klecl)
  #:use-module (emacsy kbd-macro)
  #:use-module (emacsy minibuffer)
  #:use-module (emacsy coroutine)
  #:use-module (emacsy agenda)
  #:replace (switch-to-buffer)
  #:export (read-from-mouse))
;;; <core:macro>=                                                           
(define-syntax-public track-mouse
  (syntax-rules ()
    ((track-mouse e ...)
     (in-out-guard  ;; This is different from dynamic-wind.
       (lambda () (set! emacsy-send-mouse-movement-events? #t))
       (lambda () e ...)
       (lambda () (set! emacsy-send-mouse-movement-events? #f))))))

;;; % -*- mode: Noweb; noweb-code-mode: scheme-mode -*-                     
;;; \section{Core}                                                          
;;;                                                                         
;;; %\epigraph{...}{...}                                                    
;;;                                                                         
;;; Now we're going to put in place some core functionality that makes      
;;; Emacsy an Emacs-like library.                                           
;;;                                                                         
;;; We need a global keymap.                                                
;;;                                                                         
;;;                                                                         
;;; <core:state>=                                                           
(define-public global-map (make-keymap))
(define-public special-event-map (make-keymap))
;;; <core:state>=                                                           
(define-public emacsy-quit-application? #f)
;;; \subsection{universal-argument}                                         
;;;                                                                         
;;;                                                                         
;;; <core:state>=                                                           
;(define-parameter universal-argument-queue (make-q) "This holds the current universal argument value.")
(define universal-argument-queue (make-q))
;;; <core:state>=                                                           
(define-public buffer-classes (list <text-buffer>))
;;; \subsection{Messages Buffer}                                            
;;;                                                                         
;;;                                                                         
;;; <core:state>=                                                           
(define-public messages 
  (make <text-buffer> #:keymap (make-keymap) #:name "*Messages*"))
;;; We want to be able to define variables that are not redefined if a      
;;; source file or module is reloaded, just like [[define-once]].           
;;;                                                                         
;;; \subsection{Mouse Movement}                                             
;;;                                                                         
;;; Sometimes we may want to track the motion events generated by a mouse.  
;;; We don't do this all the time because it seems unnecessarily taxing.    
;;;                                                                         
;;;                                                                         
;;; <core:state>=                                                           
(define-public emacsy-send-mouse-movement-events? #f)
;;; <core:procedure>=                                                       
(define-public (current-minor-mode-maps)
  (list))

(define-public (current-active-maps)
  `(,(current-local-map) ,@(map mode-map (buffer-modes (current-buffer))) ,global-map))

(set! default-klecl-maps current-active-maps)
;;; <core:procedure>=                                                       
(define-public (universal-argument-ref)
  (if (q-empty? universal-argument-queue)
      1
      (q-front universal-argument-queue)))

(define-public (universal-argument-pop!)
  (if (q-empty? universal-argument-queue)
      1
      (q-pop! universal-argument-queue)))

(define-public (universal-argument-push! arg)
  (q-push! universal-argument-queue arg))
;;; <core:procedure>=                                                       
(define-interactive 
  (switch-to-buffer 
   #:optional 
   (buffer
    (let-values (((to-string from-string) (object-tracker buffer-name)))
      (from-string (completing-read "Buffer: " 
                                    (map to-string (buffer-list))))))
   (buffer-class-arg #f))
  (define (coerce buffer)
    (if (is-a? buffer <string>)
        (or (find (lambda (b) (string=? buffer (buffer-name b))) (buffer-list))
            buffer)
        buffer))
  (set! buffer (coerce buffer))
  (if (is-a? buffer <buffer>)
   ((@@ (emacsy buffer) primitive-switch-to-buffer) buffer)
   (if (is-a? buffer <string>)
    ;; Create a new buffer
    (let* ((buffer-class (or buffer-class-arg ;;; <core:Choose a buffer class.>=                                          
                                              (if (= (length buffer-classes) 1)
                                                  ;; If we only have one buffer class, use that.
                                                  (car buffer-classes)
                                                  ;; Otherwise, let the user choose one.
                                                  (let*-values 
                                                      (((to-string from-string) (object-tracker 
                                                                                 (compose symbol->string class-name))))
                                                    (let ((a-buffer-class 
                                                           (from-string (completing-read "Buffer Class: " 
                                                                                         (map to-string buffer-classes)))))
                                                      (if (is-a? a-buffer-class <class>)
                                                          a-buffer-class
                                                          (begin 
                                                            (message "No such class ~a." a-buffer-class)
                                                            (throw 'invalid-class))))))))
           (new-buffer ;;; %\todo{XXX Should define-interactive export the symbol? NO.}            
                       ;;;                                                                         
                       ;;; We will need to register classes that will be available to the user to  
                       ;;; instantiate.                                                            
                       ;;;                                                                         
                       ;;;                                                                         
                       ;;; <core:Create a new buffer of a certain class.>=                         
                       (make buffer-class #:name buffer)))
          (add-buffer! new-buffer)
          new-buffer)
    (begin (message "Buffer or buffer-name expected.")
           #f))))
;;; <core:procedure>=                                                       
(define echo-area "")

(define-public (emacsy-echo-area)
  echo-area)

(define-public (current-message)
  echo-area)

(define (emacsy-message . args)
  (let ((string (apply format #f args)))
   (with-buffer messages
                (insert string)
                (insert "\n"))
   (set! echo-area string)
   (if emacsy-interactive?
       (wait)
       (display string))
   string))

;; There's probably a better way to do this.
(set! message emacsy-message)
;;; When the minibuffer is entered, we want to clear the echo-area.         
;;; Because the echo-area is defined in core, it seems best to deal with    
;;; it in core rather than placing echo-area handling code in minibuffer.   
;;;                                                                         
;;;                                                                         
;;; <core:procedure>=                                                       
(define-public (clear-echo-area)
  (set! echo-area ""))
;;; These are most of the C API calls.                                      
;;;                                                                         
;;;                                                                         
;;; <core:procedure>=                                                       
(define-public (emacsy-message-or-echo-area)
  (if emacsy-display-minibuffer?
      (buffer-string minibuffer)
      echo-area))

(define-method-public (emacsy-mode-line)
  (emacsy-mode-line (current-buffer)))

;; XXX this should be moved into the (emacsy buffer) module.
(define-method-public (emacsy-mode-line (buffer <buffer>))
  (format #f "-:**- ~a    (~{~a~^ ~})" (buffer-name buffer) (map mode-name (buffer-modes buffer))))

(define-public (emacsy-minibuffer-point)
  (if emacsy-display-minibuffer?
      (point minibuffer)
      -1))

(define-public (emacsy-run-hook hook . args)
  (catch #t
    (lambda () 
      (if debug-on-error?
          (call-with-error-handling
           (lambda ()
             (apply run-hook hook args)
             #t))
          (with-backtrace* 
           (lambda ()  
             (apply run-hook hook args)
             #t))))
    (lambda (key . args)
      (emacsy-log-error "Hook ~a threw error '~a with args ~a " hook key args)
;      (emacsy-log-error "Resetting hook ~a" hook)
;      (reset-hook! hook)
      #f)))

(define-public emacsy-terminate-hook (make-hook))

(define-public (emacsy-terminate)
  (run-hook emacsy-terminate-hook))

(define-public (emacsy-tick)
  (define (my-tick)
    (update-agenda))
  
  (if debug-on-error?
      (call-with-error-handling
       (lambda ()
         (my-tick)))
      (with-backtrace* 
       (lambda ()  
         (my-tick)))))
;;; <core:procedure>=                                                       
(define* (read-from-mouse #:optional (prompt #f))
  (define (my-read-event) 
    (if (and (pair? this-command-event)
             (mouse-event? (car this-command-event)))
        (let ((event (car this-command-event)))
          (set! this-command-event (cdr this-command-event))
          event)
          ;; XXX Should this be read-key or read-event?
        (read-event prompt)))
  (let loop ((event (my-read-event)))
    (if (mouse-event? event)
        ;; Got an event.
        (position event)
        (let ((canceled? #f))
          ;; Put this event back in the queue.  
          (emacsy-event event)
          (catch
            'quit-command
            (lambda () (primitive-command-tick))
            (lambda (key . args)
              (emacsy-log-debug "READ-FROM-MOUSE CANCELED\n")
              (set! canceled? #t)))
          (if canceled?
              (throw 'quit-command 'quit-read-from-mouse)
              (loop (my-read-event)))))))
;;; \subsection{Command Loop}                                               
;;;                                                                         
;;; If we ever run out of command loops due to errors, we start a new one.  
;;;                                                                         
;;;                                                                         
;;; <core:procedure>=                                                       
(codefine (restart-command-loop)
  ;; Start another command-loop
          (while #t
            (emacsy-log-warning "NO COMMAND LOOPS; STARTING ANOTHER.")
            (command-loop)))

(codefine (non-interactive-command-loop)
  ;; Start another command-loop
          (emacsy-log-warning "STARTING NON-INTERACTIVE COMMAND LOOP.")
          (catch #t 
            (lambda () (primitive-command-loop))
            (lambda args
              (format #t "stopping non-interactive command loop ~a" args)))
          (exit 0)
          ;;(quit-application)
          )

;(agenda-schedule restart-command-loop)

(define-public (emacsy-initialize interactive?)
  (when (and #f interactive?)
    (agenda-schedule restart-command-loop))
  (agenda-schedule (if interactive? 
                       restart-command-loop
                       non-interactive-command-loop))
  (set! emacsy-interactive? interactive?))
;;; \subsection{eval-expression}                                            
;;; There is one command that I consider fundamental for an Emacs-like      
;;; program.  Whenever I'm presented with a program that claims to be       
;;; Emacs-like, I try this out \verb|M-: (+ 1 2)|. If it doesn't work then  
;;; it may have Emacs-like key bindings, but it's not Emacs-like.  That     
;;; command is [[eval-expression]].  Let's write it.                        
;;;                                                                         
;;;                                                                         
;;; <core:command>=                                                         
(define-interactive 
  (eval-expression
   #:optional 
   (expression (read-from-string 
               (completing-read "Eval: " apropos-completion-function))))
  (let ((value (eval expression (interaction-environment))))
    (message "~a" value)
    value))
;;; \subsection{execute-extended-command}                                   
;;;                                                                         
;;; The second fundamental command is [[execute-extended-command]] invoked  
;;; with \verb|M-x|.                                                        
;;;                                                                         
;;;                                                                         
;;; <core:command>=                                                         
(define-interactive (execute-extended-command #:optional (n 1))
  ;(display "HERE!\n")
  (let ((str (completing-read "M-x " (completer global-cmdset))))
    (call-interactively (module-ref (current-module) (string->symbol str)))))
;;; <core:command>=                                                         
(define-interactive (quit-application)
  (set! emacsy-quit-application? #t)
  (wait))
;;; This [[universal-argument]] command is written using a different style  
;;; than is typical for interative Emacs commands.  Most Emacs commands     
;;; are written with their state, keymaps, and ancillary procedures as      
;;; public variables.  This style has a benefit of allowing one to          
;;; manipulate or extend some pieces; however, there are some benefits to   
;;; having everything encapsulated in this command procedure.  For          
;;; instance, if the minibuffer were written in this style, one could       
;;; invoke recursive minibuffers.                                           
;;;                                                                         
;;;                                                                         
;;; <core:command>=                                                         
(define-interactive (universal-argument)
  "Universal argument is used as numerical input for many functions."
  (let ((count 0)
        (ua-keymap (make-keymap))
        (prompt "C-u ")
        ;(acceptable-chars (char-set-adjoin char-set:digit #\-))
        (done? #f))
    (define (add-to-prompt string)
      (set! prompt (string-concatenate (list prompt string " "))))
    (define (process-arg number)
      (add-to-prompt (number->string number))
      (set! count (+ (* count 10) number)))
    (define (my-undefined-command key-sequence events)
      (if (= count 0)
          (universal-argument-push! 4)
          (universal-argument-push! count))
      (set! done? #t)
      (set! echo-area "") ;; Clear the echo area.
      (for-each emacsy-event-unread (reverse events)))
    (define-key ua-keymap "1" (lambda () (process-arg 1)))
    (define-key ua-keymap "2" (lambda () (process-arg 2)))
    (define-key ua-keymap "3" (lambda () (process-arg 3)))
    (define-key ua-keymap "4" (lambda () (process-arg 4)))
    (define-key ua-keymap "5" (lambda () (process-arg 5)))
    (define-key ua-keymap "6" (lambda () (process-arg 6)))
    (define-key ua-keymap "7" (lambda () (process-arg 7)))    
    (define-key ua-keymap "8" (lambda () (process-arg 8)))
    (define-key ua-keymap "9" (lambda () (process-arg 9)))
    (define-key ua-keymap "0" (lambda () (process-arg 0)))
    (define-key ua-keymap "-" (lambda () 
                                (set! count (- count))
                                (add-to-prompt "-")))
    (define-key ua-keymap "C-u" (lambda () 
                                  (when (= count 0)
                                    (set! count 4))
                                  (set! count (* count 4))
                                  (add-to-prompt "C-u")))
    (while (not done?)
      (primitive-command-tick prompt 
                              #:keymaps (list ua-keymap)
                              #:undefined-command my-undefined-command))
    (when #f
      (let loop ((input (read-key prompt)))
          ;; We only admit numbers and a dash
          (if (and (is-a? input <key-event>)
                   (null? (modifier-keys input))
                   (char-set-contains? acceptable-chars (command-char input)))
              (begin
                (cond
                 ((char=? (command-char input) #\-)
                  (add-to-prompt "-")
                  (set! count (- count)))
                 (else
                  (process-arg (string->number (string (command-char input))))))
                (loop (read-key prompt)))
              (begin
                (if (= count 0)
                    (universal-argument-push! 4)
                    (universal-argument-push! count))
                (emacsy-event-unread input)))))))
;;; We want to be able to load a scheme file.  %\todo{We should have a      
;;; %  read-filename procedure.}                                            
;;;                                                                         
;;;                                                                         
;;; <core:command>=                                                         
(define-interactive 
  (load-file #:optional (filename (read-file-name "Filename: ")))
  (catch #t
    #.\ (begin (load filename)
               (message "Loaded ~a." filename)
               #t)
    (lambda (key . args)
      (let ((error-msg 
             (call-with-output-string 
              #.\ (apply display-error #f % args))))
        (message "Failed to load ~a: ~a" filename error-msg)
        #f))))
;;; And here are the essential key bindings.                                
;;;                                                                         
;;;                                                                         
;;; <core:keybinding>=                                                      
(define-key global-map "M-:"        'eval-expression)
(define-key global-map "M-x"        'execute-extended-command)
(define-key global-map "C-g"        'keyboard-quit)
(define-key global-map "C-x C-c"    'quit-application)
(define-key global-map "C-u"        'universal-argument)

(define-key special-event-map "C-g" 'keyboard-quit)
;;; \subsubsection{Keyboard Macro Keybindings}                              
;;;                                                                         
;;;                                                                         
;;; <core:keybinding>=                                                      
(define-key global-map "C-x (" 'kmacro-start-macro)
(define-key global-map "C-x )" 'kmacro-end-macro)
(define-key global-map "C-x e" 'kmacro-end-and-call-macro)
;;; \subsubsection{Buffer Manipulation Keybindings}                         
;;;                                                                         
;;;                                                                         
;;; <core:keybinding>=                                                      
(define-key global-map "C-o"   'other-buffer)
(define-key global-map "C-x k" 'kill-buffer)
(define-key global-map "C-x b" 'switch-to-buffer)
;;; <core:process>=                                                         
(add-buffer! messages)
;;; <core:process>=                                                         
(add-hook! (buffer-enter-hook minibuffer)
           (lambda () (clear-echo-area)))
;;; <core:process>=                                                         
(add-hook! no-blocking-continuations-hook restart-command-loop)
