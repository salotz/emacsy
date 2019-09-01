;; Copyright (C) 2019 Amar Singh

;; Module --- Desc

;; This file is part of Module.

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

(define-module (emacsy monad-repl)
  #:use-module (emacsy monad)
  #:use-module (emacsy buffer)
  #:use-module (ice-9 pretty-print)
  #:use-module (system repl repl)
  #:use-module (system repl common)
  #:use-module (system repl command)
  #:use-module (system base language)
  #:use-module (system base compile)
  #:export (
            enter-monad
            ))

;;; monad repl from guix monad-repl
;;; Thanks Guix and ludovic

(define* (monad-language monad run #:optional (name 'monad))
  "Return a language with a special evaluator that causes monadic values
to be 'run' in MONAD using procedure RUN."
  (let ((scheme (lookup-language 'scheme)))
    (define (evaluate-monadic-expression exp env)
      (let ((mvalue (compile exp #:to 'value #:env env)))
        (run mvalue)))

    (make-language #:name name
                   #:title "Monad"
                   #:reader (language-reader scheme)
                   #:compilers (language-compilers scheme)
                   #:decompilers (language-decompilers scheme)
                   #:evaluator evaluate-monadic-expression
                   #:printer (language-printer scheme)
                   #:make-default-environment
                   (language-make-default-environment scheme))))

(define (buffer-stack-monad-language buffer-stack)
  "Return a compiler language for the buffer-stack monad using STORE."
  (monad-language buffer-stack
                  buffer-monad-run))

(define-meta-command ((enter-monad buffers) repl)
  "run-in-store EXP
Run EXP through the store monad."
  (let ((buffer-stack (buffers)))
    (let ((new (make-repl (buffer-stack-monad-language buffer-stack))))
      (repl-option-set! new 'interp #t)
      (run-repl new))))
;;; usage (buffer-stack (msend add-buffer (make <buffer> #:name "1"))
;;; (msend add-buffer (make <buffer> #:name "2")))
;; $9 = <mru-stack (#<buffer 2> #<buffer 1>)>
