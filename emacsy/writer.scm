;; Copyright (C) 2019  Amar Singh

;; Writer --- Disk language

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

(define-module (emacsy writer)
  #:use-module (emacsy monad)
  #:use-module (emacsy circular))

(define-macro (define-file file val)
  `(call-with-output-file ,(symbol->string file)
    (lambda (port)
      (write ,val port))))

(export-syntax define-file)
