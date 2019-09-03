;; Copyright (C) 2019  Amar Singh

;; EDSL --- Extensible Denotational Language specifications

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

(define-module (emacsy edsl))

;;; general stateful behaviour
(define-public val 0)

(define-public (add . rest)
  (set! val (apply + val rest)))

(define-public (tract . rest)
  (set! val (apply + val rest)))

;;; extensible effects
(define-public cval 0)

(define-public admin
  (lambda (tag val1 val2)
    (set! val1 val2)
    (pk val1)
    (pk val2)
    (abort-to-prompt tag)))

(define-public (madd . rest)
  (call-with-prompt 'madd
    (lambda ()
      (abort-to-prompt 'madd
                       (set! cval (apply + cval rest))))
    (lambda (k)
      (pk k)
      k)))

;;; felleisen cartwright 1994, edls
;;; Problema:
;;; Functional lang: Numeral :: Environment -> Integer
;;; Functional lang + imperative control: Numeral :: Environments x
;;; Continuations -> Integer

;;; Shiny thing: Extended direct semantics (woot woot!)
;;; 1. add orthogonal extensions to language.
;;; 2. without changing the denotations of existing language phrases.

;;; Observations:
;;; 1. an extended direct semantics always maps a numeral to the same
;;; denotation; injection of corresponding number into domain of values.

;;; 2. In general, the denotation of a phrase in a functional language
;;; is always a projection of the denotation of the same phrase in the
;;; semantics of an extended language.

;;; 3. Based on extended direct semantics, it is also possible to
;;; construct interpreters for complete languages by composing
;;; interpreters for language fragments.

;;; Denotational specs of a complex languages: A lang like schemer,
;;; common lisp or ml consists of a rich functional core, augmented by
;;; destructive operations on data objects, control constructs, and
;;; possibly other imperative operators. Traditional denotational lang
;;; specs cope with these constructs by interpreting program phrases as
;;; functions that map 'environments x stores x continuations' to
;;; 'values x stores'. Programmers, however rely n simpler semantic
;;; descriptions when they reason about program phrases. Most program
;;; phrases do not exploit the full generality of the language,
;;; permitting their semantics to be analyzed using a simpler semantic
;;; model.

;;; TLDR0; Powerful programming languages introduce all types of
;;; constructs, functional, imperative, control structs, in doing so
;;; they give up the simpler semantics of the less-powerful and less
;;; feature-full lang.

;;; Pour example, if a program phrase is purely functional and its free
;;; variables are always bound to effect-free procedures, it can be
;;; interpreted as a function mapping 'environments' to
;;; 'values'. Similarly, if an imperative program phrase does not use
;;; general control operators like callcc, goto, or catch and its free
;;; variables are always bound to values and procedures conforming to
;;; the same constraint, it can be interpreted as a function mapping
;;; 'environments x stores' to 'values x stores'. Unfortunatly,
;;; denotational defs oof practical programming langs are written in a
;;; form that makes it difficult to extract a simplified def for a
;;; disciplined subset--much less prove that the def are equivalent over
;;; the restricted lang.

;;; TLDR1; extended lang semantics allow us to use simpler reasoning if
;;; we know that a program does not use all the lang feature but only a
;;; subset of it. For example, if you know that a program does not use
;;; imperative constructs, then you can feel safer about your program.

;;; another

(define-public (diverge)
  "Diverge is a program that always diverges."
  (diverge))

(define-public err error)

(define-public admin
  (lambda (action handle)
    (action)
    (handle)))

(define-public compute
  (lambda ()
    (do-some-action) ;; effects
    (create-a-value) ;; functional
    ))
