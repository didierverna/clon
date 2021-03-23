;;; net.didierverna.clon.asd --- ASDF system definition

;; Copyright (C) 2010-2012, 2015, 2021 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(defsystem :net.didierverna.clon
  :long-name "The Command-Line Options Nuker"
  :description
  "Command-line options management for standalone Common Lisp applications"
  :long-description "\
Clon is a library for command-line options management. It is intended to ease
the creation of standalone Common Lisp applications by providing a powerful
and uniform command-line options interface. The most important features of
Clon are the following.

  - From the application programmer's point of view: centralized command-line
  options specification and management, including automatic generation of help
  strings, conversion from command-line / defaults / fallbacks / environment
  variables to application-level option values, global or on-demand option
  retrieval, and extensibility (the programmer can define his own option
  types).

  - From the application user's point of view: uniform command-line option
  syntax across all Clon applications, customization of the help strings
  layout (with optional ISO6429 coloring on terminals that support it),
  automatic completion of abbreviated option names and short/long/pack syntax."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/clon.php"
  :source-control "https://github.com/didierverna/clon"
  :license "BSD"
  :defsystem-depends-on (:net.didierverna.clon.setup/termio)
  :version (:read-file-line #p"make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on (:net.didierverna.clon.core
	       (:feature :net.didierverna.clon.termio
		:net.didierverna.clon.termio)))

;;; net.didierverna.clon.asd ends here
