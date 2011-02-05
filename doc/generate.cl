;;; generate.cl --- Clon reference manual generation script

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Clon.

;; Redistribution and use in source or binary form, with or without
;; modification, are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.

;; Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; Neither the names of the authors or copyright holders, nor the names of any
;; contributor or organization may be used to endorse or promote products
;; derived from Clon without specific prior written permission.

;; CLON IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(require :asdf)

(defconstant +introduction+
  "@macro clon
@t{Clon}
@end macro

@macro cmdline
command-line
@end macro

@macro CmdLine
Command-Line
@end macro

@macro cl
Common-Lisp
@end macro

@macro tty
@t{tty}
@end macro

@macro etc
@i{etc.}
@end macro

@macro pxenduserref{node}
@ifinfo
@pxref{\\node\\, , , clon-enduser, The Clon End-User Manual}
@end ifinfo
@ifnotinfo
@pxref{\\node\\, , , enduser, The Clon End-User Manual}
@end ifnotinfo
@end macro

@macro pxuserref{node}
@ifinfo
@pxref{\\node\\, , , clon-user, The Clon User Manual}
@end ifinfo
@ifnotinfo
@pxref{\\node\\, , , user, The Clon User Manual}
@end ifnotinfo
@end macro

@clon{} is a library for managing @cmdline{} options in standalone @cl{}
applications. It provides a unified option syntax with both short and
long names, automatic completion of partial names and automatic
retrieval/conversion of option arguments from the @cmdline{}, associated
environment variables, fallback or default values. @clon{} comes with a
set of extensible option types (switches, paths, strings @etc{}).
@clon{} also provides automatic generation and formatting of help
strings, with support for highlighting on @tty{}'s through ISO/IEC 6429
SGR. This formatting is customizable through @emph{themes}.

Depending on the target audience, @clon{} stands for either ``The
@CmdLine{} Options Nuker'' or ``The @cl{} Options Nuker''. @clon{} also
has a recursive acronym: ``@clon{} Likes Options Nuking'', and a reverse
one: ``Never Omit to Link with @clon{}''. Other possible expansions of
the acronym are still being investigated.

This is the @clon{} reference manual, and as such, it is not meant to be
read. It may help you find sleep in case of insomnia though. @clon{}
comes with two human-readable manuals:
@itemize @bullet
@item
the ``end-user manual'' (@pxenduserref{Top}) is for the @clon{}
@emph{end-user}, that is, the user of an application powered by @clon{}.
It describes how to use the @cmdline{} of clonified@footnote{An
application using @clon{} for its @cmdline{} option management is said to
be @emph{clonified}. It is also possible to say @emph{clonfiscated}.
However, we advise against using @emph{clonistified}. The term
@emph{clonificated} is also considered bad style, and the use of
@emph{clonificationated} is strictly prohibited.} applications and how to
customize @clon{}'s output. Everybody should read this manual first.
@item
the ``user manual'' (@pxenduserref{Top}) is for the @clon{} @emph{user},
that is, the developer of a @cl{} application who wants to use @clon{} for
@cmdline{} option management. It describes how to clonify your application
and extend the library with your own option types.
@end itemize"
  "The reference manual's introductory text.")

(asdf:operate 'asdf:load-op :com.dvlsoft.declt)

(if (and (second sb-ext:*posix-argv*)
	 (string= (second sb-ext:*posix-argv*) "--web"))
    (com.dvlsoft.declt:declt :com.dvlsoft.clon
			     :library-name "Clon"
			     :texi-file "webreference.texi"
			     ;; but we don't care
			     :info-file "clon-webreference"
			     :introduction +introduction+
			     :link-files nil)
  (com.dvlsoft.declt:declt :com.dvlsoft.clon
			   :library-name "Clon"
			   :texi-file "reference.texi"
			   :info-file "clon-reference"
			   :introduction +introduction+))

(sb-ext:quit)


;;; generate.cl ends here
