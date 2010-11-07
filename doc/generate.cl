;;; generate.cl --- Clon reference manual generation script

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Sep 21 15:26:35 2010
;; Last Revision: Sun Nov  7 21:07:47 2010

;; This file is part of Clon.

;; Clon is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; Clon is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


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
