% hello.w
  
\documentclass[twocolumn]{report}
\newif\ifshowcode
\showcodetrue

\usepackage{etoolbox}
\newtoggle{proposal}
\togglefalse{proposal}
\usepackage{verbatim}
\usepackage{latexsym}
\usepackage{amsmath}
\usepackage{graphicx}
%\usepackage{url}
%\usepackage{html} 

\usepackage{listings}
\usepackage{minted}
\usemintedstyle{monokai}
\definecolor{bg}{RGB}{39,40,34}

\usepackage{color}
%\usepackage{framed}
\usepackage{textcomp}
%\definecolor{listinggray}{gray}{0.9}
%\definecolor{shadecolor}{HTML}{211e1e}
\lstset{
	tabsize=2,
	language=lisp,
    keepspaces=true,
    upquote=true,
    aboveskip=0pt,
    belowskip=0pt,
    framesep=0pt,
    rulesep=0pt,
    columns=fixed,
    showstringspaces=true,
    extendedchars=true,
    breaklines=true,
    prebreak = \raisebox{0ex}[0ex][0ex]{\ensuremath{\hookleftarrow}},
    frame=none,
    framerule=0pt,
    showtabs=false,
    showspaces=false,
    showstringspaces=false,
    keywords={lambda, define, define-syntax, syntax-rules, set, while, if, begin, define-module, use-module, use-modules, let, let*, }
    %basicstyle=\color[HTML]{dadada},
	%rulecolor=\color[HTML]{dadada},
	%backgroundcolor=\color[HTML]{211E1E},
    %identifierstyle=\color[HTML]{bec337},%\ttfamily,
    %keywordstyle=\color[HTML]{6f61ff},
    %commentstyle=\color[HTML]{ED5B15},
    %stringstyle=\color[HTML]{ad9361}
}
\definecolor{linkcolor}{rgb}{0, 0, 0.7}
\usepackage[backref,raiselinks,pdfhighlight=/O,pagebackref,hyperfigures,breaklinks,colorlinks,pdfstartview=FitBH,linkcolor={linkcolor},anchorcolor={linkcolor},citecolor={linkcolor},filecolor={linkcolor},menucolor={linkcolor},pagecolor={linkcolor},urlcolor={linkcolor}]{hyperref}
\NWuseHyperlinks
%\renewcommand{\NWtarget}[2]{\hypertarget{#1}{#2}}
%\renewcommand{\NWlink}[2]{\hyperlink{#1}{#2}} 
\renewcommand{\NWtxtDefBy}{defined by}
\renewcommand{\NWtxtRefIn}{referenced in}
\renewcommand{\NWtxtNoRef}{not referenced}
\renewcommand{\NWtxtIdentsUsed}{Fragment uses}
\renewcommand{\NWtxtIdentsNotUsed}{(never used)}
\renewcommand{\NWtxtIdentsDefed}{Fragment defines}

\setlength{\oddsidemargin}{0in}
\setlength{\evensidemargin}{0in}
\setlength{\topmargin}{0in}
\addtolength{\topmargin}{-\headheight}
\addtolength{\topmargin}{-\headsep}
\setlength{\textheight}{8.9in}
\setlength{\textwidth}{6.5in}
\setlength{\marginparwidth}{0.5in}

%\newminted{scheme}{bgcolor=bg,mathescape,texcl}

%\title{Emacsy: An Extensible Macro System}
\title{Emacsy: An Extensible, Embedable, Emacs-like Macro System}
%\title{Emacsey: An Embedable Macro System for Embedding }
\date{}
\author{Shane Celis
\\ {\sl shane.celis@@gmail.com}}

\input{commands}

\begin{document}
\maketitle
%\chapter{Introduction}
\section{Introduction}
% What's the logo?  How about a gnu inside some other creature?
% Think an O'Reily animal with a gnu inside it.

Emacsy is inspired by the Emacs text editor, but this project is not
an attempt to create yet another text editor.  This project is an attempt
to extract the kernel of Emacs' extensible key binding system for easy
use within other programs that have nothing to do with text editing.

\begin{comment}

\footnote{There is a joke
  that ``Emacs is a great operating system---it lacks a good editor,
  though.'' Funnily enough, Emacsy is like Emacs but without any
  editor.}
\end{comment}

\subsection{Vision}

Emacs has been extended to do much more than text editing.  It can get
your email, run a chat client, do video
editing\footnote{\url{http://1010.co.uk/gneve.html}}, and more.  For
some the prospect of chatting from within one's text editor sounds
weird.  Why would anyone want to do that?  Because Emacs gives them so
much control.  Frustrated by a particular piece of functionality?
Disable it.  Unhappy with some unintuitive key binding?  Change it.
Unimpressed by built-in functionality?  Rewrite it.

The role I see for Emacsy is to bring the Emacs way of doing things to
whatever application one is working on.  In my mind, I imagine Emacs
consuming applications from the outside, while Emacsy subsumes
applications from the inside---thereby allowing many applications to
be Emacs-like without requiring each to use Emacs as their front-end.
I would like to hit \verb|M-x| in other applications to run
commands. I would like to see authors introduce a new version:
``Version 3.0, now with Emacsy extensibility.''  I would like hear
power users ask, ``Yes, but is it Emacsy?''

\subsection{Motivation}

This project was inspired by frustration with creating interactive
applications with the conventional edit-run-compile style of
development.  Finding the right abstraction for the building blocks of
the User Interface (UI) such that they compose well is not easy.  This
problem is exacerbated if the application is a means to an end and not
an end in itself.

\begin{figure}
  \centering
  \includegraphics[scale=0.4]{emacsy-logo.pdf} 
  \caption[Short Label]{\label{emacsy-logo}The proposed logo features
    a small gnu riding an elephant.}
\end{figure}


\subsection{Overlooked Treasure}

Emacs has a powerful means of programmatically extending itself while
it is running.  Not many successful applications can boast of that,
but I believe a powerful idea within Emacs has been overlooked as an
Emacsism rather than an idea of general utility.  Let me mention
another idea that might have become a Lispism but has since seen
widespread adoption.

The Lisp programming language introduced the term Read-Eval-Print-Loop
(REPL, pronounced rep-pel), an interactive programming feature present
in many dynamic languages: Python, Ruby, MATLAB, Mathematica, Lua to
name a few.  The pseudo code is given below.

@{
(while #t
 (print (eval (read))))
@}

The REPL interaction pattern is to enter one complete expression, hit
the return key, and the result of that expression will be displayed.
It might look like this:

some

other stuff


\begin{verbatim}
> (+ 1 2)
3
\end{verbatim}\label{blah}

%% \begin{minted}[bgcolor=bg,mathescape,texcl]{c}
%% // Ou, I like!  3
%% int main() {
%%   // $\text{< other stuff > figure. \ref{blah}} \ref{blah}$
%%   printf("Hello, world!");  

%%   return 0;
%% } 
%% \end{minted} 

The kernel of Emacs is conceptually similar to the REPL, but the level
of interaction is more fine grained.  A REPL assumes a command line
interface.  Emacs assumes a keyboard interface.  I have not seen the
kernel of Emacs exhibited in any other applications, but I think it is
of similar utility to the REPL---and entirely separate from text
editing.  I'd like to name this the Key-Lookup-Execute-Command-Loop
(KLECL, pronounced clec-cull).

@{
(while #t
 (execute-command (lookup-key (read-key))))
@}

Long-time Emacs users will be familiar with this idea, but new Emacs
users may not be.  For instance, when a user hits the 'a' key, then an
'a' is inserted into their document.  Let's pull apart the functions
to see what that actually looks like with respect to the KLECL.


\begin{verbatim}
> (read-key)
#\a
> (lookup-key #\a)
self-insert-command
> (execute-command 'self-insert-command)
#t
\end{verbatim}

Key sequences in Emacs are associated with commands.  The fact that
each command is implemented in Lisp is an implementation detail and
not essential to the idea of a KLECL.


\subsection{Goals}

The goals of this project are as follows.

\begin{enumerate}
\item Easy to embed technically

  Emacsy should be easy to embed within an existing project.  (The
  implementation choices will necessarily restrict it to perhaps only
  C and C++ programs initially.)

\item Easy to embed legally

  My preference for Emacsy in terms of its license would be something
  like the Lesser GNU Public
  License\footnote{\url{http://www.gnu.org/licenses/lgpl-3.0.txt}}.
  Something one can use free of contamination, but where changes to
  Emacsy in particular are protected by a license that requires those
  changes be open.

\item Small enough to understand and trust

  It ought to be small enough that one can wrap their hands and mind
  around it such that one is able trust it, which is important for
  embedding.  (This document is an effort towards this goal.)

\item Easy to learn

  Emacsy should be easy enough to learn that the uninitiated may easily
  make parametric changes, e.g., key 'a' now does what key 'b' does
  and \emph{vice versa}.  Programmers in any language ought to be able
  to make new commands for themselves.  And old Emacs hands should be
  able to happily rely on old idioms and function names to change most
  anything.

\item Opinionated but not unpersuadable

  Emacsy should be configured with a sensible set of defaults
  (opinions).  Out of the box, it is not \emph{tabla rasa}, a blank
  slate, where the user must choose every detail, every time.
  However, if the user wants to choose every detail, they can.

\item Key bindings can be modified

\item Commands can be defined in Emacsy's language or the host
  language

\item Commands compose well
  
  That is to say, commands can call other commands.  No special
  arrangements must be considered in the general case.

\item A small number of \emph{interface} functions

  The core functions that must be called by the embedding application
  ought to be small and relatively straightforward.  To that end, all
  these \emph{interface} variables and functions that are expected to
  be called have the prefix 'emacsy-' in their names.

\item Bring KLECL to light

\end{enumerate}

\subsection{Anti-goals}

Just as important as a project's goals are its anti-goals: the things
it is not intended to do.

\begin{enumerate}
\item Not a general purpose text editor

  Emacsy will not do general purpose text editing out of the box.  It
  will support a minibuffer.

\item Not an Emacs replacement

  There have been many attempts to replace Emacs and elisp with an
  newer Lisp dialect.  Emacsy is not one of them.  Emacs is full
  featured programmer's text editor with more bells and whistles than
  most people will ever have the time to fully explore.

\item Not source code compatible with Emacs

  Although Emacsy may adopt some of naming conventions of Emacs, it
  will not use elisp and will not attempt to be in any way source code
  compatible with Emacs.

\item Not a framework
  
  I will not steal your runloop.  You call Emacsy when it suits your
  application not the other way around.

\end{enumerate}

%\section{Usage}

%% \section{Implementation Plan}

%% One could go about implementing a project with the aforementioned
%% goals in many different ways.  Althought, I am interested in
%% separating the KLECL from Emacs in particular, for instance I think
%% implementing a KLECL in \verb|Lua| would be valuable, but I also want
%% more than just the KLECL.  I want to provide an Emacs-like way of
%% developing applications for developers of all stripes.

%% \subsection{Implementation Decisions}

%% \begin{enumerate}
%% \item Use GNU Guile Scheme.

%% \item Adopt Emacs naming conventions.

%% \item Write as a literate program.

%%   To assist in the goal of making this project understandable enough
%%   to trust, I plan to use
%%   \verb|nuweb|\footnote{\url{http://nuweb.sourceforge.net/}} and write
%%   this in literate programming fashion where the source code and
%%   documentation come from the same document.  

%% %\item Make code available on github
%% \end{enumerate}

\subsection{How you can help?}

\begin{enumerate}
\item Tell other people about Emacsy, especially other Emacs users and
  developers.
\item Tell me how to make the project better.  
\begin{itemize}
\item  You can send me an email: \href{mailto:shane.celis@@gmail.com}{shane.celis@@gmail.com}.
\item  You can leave a comment on kickstarter.
  
\item  You can vote on features at \url{http://reddit.com/r/emacsy}.

\item You can check out the latest code from \url{http://github.com/secelis/emacsy}.
\end{itemize}

\item Become a supporter on kickstarter.

  \begin{itemize}
    \item \$1 - If funded, you get a copy of Emacsy that you can run,
      modify, and redistribute (LGPL).
    \item \$10 - All the above and you are mentioned in credits.
    \item \$20 - All the above and you will get receive ``backer'' flair on
      \href{http://reddit.com/r/emacsy}{r/emacsy}.
    \item \$150 - All the above and you will receive a printed manual (US only).
    \item \$200 - All the above and if you send me your project logo,
      I will attempt to add a small gnu to it.
  \end{itemize}
\end{enumerate}

\subsection{Minimal Emacsy Example}

\section{Implementation}

The file \verb|emacsy.scm| is the heart of implementation.  The file
\verb|emacsy-tests.scm| houses all the unit tests.

@o emacsy.scm -cl -d  @{@< Lisp File Header @> 
(define-module (emacsy)
)
@< Definitions @>
@|@}

We will use the module check for most of our unit test needs.

@o emacsy-tests.scm -cl -d  @{@< Lisp File Header @>  
@< Test Preamble @>
(eval-when (compile load eval)
           (module-use! (current-module) (resolve-module '(emacsy)))) 
@< Tests @> 
@< Test Postscript @> 
@|@}

The header for Lisp files shown below.

@d Lisp File Header @{;;; @f 
;; DO NOT EDIT - generated from emacsy.w. 
;;
;; @< License @>
;;
;; @f written by Shane Celis 
;; shane (dot) celis (at) uvm (dot) edu
@|@}

@d Definitions @{@%
(define (f x)
  (1+ x))
@|f@}

@d Tests @{@%
(define-test (test-f)
  (check (f 1) => 2)
  (check (f 0) => 1)
)
@|test-f@}

\subsection{Literate Programming Support}

All the code for this project is generated from \verb|emacsy.w|.  To
ease debugging, it is helpful to have the debug information point to
the place it came from in \verb|emacsy.w| and not whatever source file
it came from.  The program \verb|nuweb| has a means of providing this
information that works for C/C++, the line pragma.  Scheme does not
support the line pragma, but the reader can fortunately be extended to
support it.

An example use of it might look like this:

@{@%
(define (f x)
#line 314 "increment-literately.w"
  (+ x 1)) @}

@d Line Pragma Handler @{@%
(lambda (char port)
  (let* ((ine (read port))
         (lineno (read port))
         (filename (read port)))
    (if (not (eq? ine 'ine))
        (error (format #f "Expected '#line <line-number> <filename>'; got '#~a~a ~a \"~a\"'." char ine lineno filename)))
    (set-port-filename! port filename)
    (set-port-line! port lineno)
    (set-port-column! port 0)
    ""))
@|@}

One problem that popped up was I sometimes wanted to include pieces of
documentation in embedded strings.  Something that might end up
looking like this in the source code:

@{
(define (f x)
  "#line 352 "emacsy.w"
   The function f and its associated function h...
   #line 362 "emacsy.w"
  "
  ...
@}

The above code will see a string "\#line 352 " followed by a bare
symbol emacsy.w, which will not do.  To get around this, I implemented
another reader extension that will strip out any \#l lines within it.

@d Liberal String Quote Reader @{@%
(lambda (char port)
  (let ((accum '()))
    (let loop ((entry (read-char port)))
      (if (or (eof-object? entry)
              (and (char=? #\" entry)
                   (char=? #\# (peek-char port))
                   (begin (read-char port)
                          #t)))
          ;; We're done
          (apply string (reverse accum))
          (begin
            (if (and (char=? #\# entry)
                     (char=? #\l (peek-char port)))
                ;; Drop this line
                (begin (read-line port)
                       (loop (read-char port)))
                (begin
                  ;; Keep and loop
                  (set! accum (cons entry accum))
                  (loop (read-char port)))))))))
@|@}

@o my-line-pragma.scm -cl @{@%
(define-module (my-line-pragma)
  #:use-module (ice-9 rdelim))

(eval-when (compile load eval)
 (read-hash-extend #\l @< Line Pragma Handler @>)
 (read-hash-extend #\" @< Liberal String Quote Reader  @>))

@|@}



\subsection{Unit Testing}

We want to be able to easily write and aggregate unit tests.  It's not
important to our project per se.  We just need the utility.  Our
association list (alist) \verb|unit-tests| will hold the symbol of the
function and the procedure.

@o check/harness.scm -cl -d @{@%
(define-module (check harness)
  #:use-module (check)
  #:export (run-tests
            run-tests-and-exit)
  #:export-syntax (define-test))
@|@}

Set up the variables.

@o check/harness.scm @{@%
(define unit-tests '())
(define test-errors '())
@|@}

We can register any procedure to a test name.  

@o check/harness.scm @{@%
(define (register-test name func)
  "Register a procedure to a test name."
  (set! unit-tests (acons name func unit-tests)))
@|@}

Typically, users will define and register their tests with this macro.

@o check/harness.scm @{@%
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name args ...) expr ...)
     (begin (define* (name args ...)
        expr ...)
     (register-test 'name name)))))
@|define-test@}

We need to run the tests.

@o check/harness.scm @{@%
(define (run-tests)
  (catch 'first-error
    (lambda () 
      @< handle each test @>)
    (lambda args
      #f)))
@|@}

@d handle each test @{@%
(for-each 
 (lambda (elt)
   (format #t "TEST: ~a\n" (car elt))
   ;;(pretty-print elt)
   (catch #t
     (lambda ()
       (with-throw-handler 
        #t
        (lambda ()
          (apply (cdr elt) '()))
        (lambda args
          (set! test-errors (cons (car elt) test-errors))
          (format #t "Error in test ~a: ~a" (car elt) args)
          (backtrace))))
     (lambda args
       (throw 'first-error)
       #f)))
 (reverse unit-tests))
@|@}

@o check/harness.scm @{@%
(define (run-tests-and-exit)
  (run-tests)
  (check-report)
  (if (> (length test-errors) 0)
      (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
      (format #t "NO ERRORs in tests."))
  (exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1)))
@|@}






@d test functions @{@%
(define unit-tests '())

(define (register-test name func)
  (set! unit-tests (acons name func unit-tests)))

@| unit-tests register-test@}

The function register-test does the work, but we don't want to require
the user to call it, so we'll define a macro that will automatically
call it.

@d test macro @{@%
(define-syntax define-test
  (syntax-rules ()
    ((define-test (name args ...) expr ...)
     (begin (define* (name args ...)
        expr ...)
     (register-test 'name name)))))
@| define-test@}

Finally, now we just need a way to run all the unit tests.

@d run tests @{@%
(define test-errors '())
(define (run-tests)
  (catch 'first-error
    (lambda () (for-each (lambda (elt)
                           (display "TEST: ")
                           (pretty-print elt)
                 (catch #t
                   (lambda ()
                     (with-throw-handler #t
                                         (lambda ()
                                           (apply (cdr elt) '()))
                                         (lambda args
                                           (set! test-errors (cons (car elt) test-errors))
                                           (format #t "Error in test ~a: ~a" (car elt) args)

                                           (backtrace))))
                   (lambda args
                     ;(throw 'first-error)
                     #f
                     )))
               (reverse unit-tests)))
    (lambda args
      #f)))
@| run-tests test-errors@}

Finally, let's provide this as our testing preamble.

@d Test Preamble @{@%
(use-modules (check))
(use-modules (ice-9 pretty-print))

@< test functions @>
@< test macro @>
@< run tests @>
@|@}

Let's run these tests at the end.

@d Test Postscript @{@%

(run-tests)
(check-report)
(if (> (length test-errors) 0)
    (format #t "~a ERROR in tests: ~a." (length test-errors) (reverse test-errors))
    (format #t "NO ERRORs in tests."))
(exit (if (and (= (length test-errors) 0) (= 0 (length check:failed))) 0 1))

@|@}

\section{Example Program: ERACS}

Extensible Robot And Controller Simulation (ERACS) was written along
side Emacsy.  

\section{Vector Math}

@o vector-math.scm -cl -d  @{@< Lisp File Header @> 
;@< Vector Module @> 
@< Vector Definitions @>
@|@}   


\begin{enumerate}
\item @< vector-component-usage @>

 The component of $\bv a$ in the $\bv b$ direction.
\begin{align*}
  \comp_\bv b \bv a &= \bv a \cdot \bhv b \\
  &= \frac{\bv a \cdot \bv b}{||\bv b||}
\end{align*}

  @d Vector Definitions @{(define (vector-component a b)
    ;(string-trim-both 
    #" @< vector-component-usage @> "# 
    ;char-set:whitespace)
 (/ (vector-dot a b) (vector-norm b)))
@|@}

@d vector-component-usage @[Scalar projection@]

\item Vector projection

  The vector projection of $\bv a$ on $\bv b$.
  \begin{align*}
    \proj_\bv b \bv a &= a_1 \bhv b \\
    a_1 &= \comp_\bv b \bv a
  \end{align*}
  @d Vector Definitions @{(define (vector-projection a b)
 (vector* (vector-component a b) (vector-normalize b)))
@|@}

\end{enumerate}







\appendix
\section{Index of Filenames}
@f
\section{Index of Fragments}
@m
\section{Index of User Specified Identifiers}
@u

\end{document}

%%  LocalWords:  elt args Emacsism lispism Eval rep pel MATLAB Lua
%%  LocalWords:  Mathematica eval int printf clec unpersuadable tabla
%%  LocalWords:  rasa Emacsy's
