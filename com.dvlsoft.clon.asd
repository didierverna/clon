;;; com.dvlsoft.clon.asd --- ASDF system definition

;; Copyright (C) 2010-2012, 2015 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of clon.

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

(asdf:load-system :com.dvlsoft.clon.setup/termio)

(asdf:defsystem :com.dvlsoft.clon
  :description "The Command-Line Options Nuker"
  :long-description "Clon is a library for command-line option management.
It is intended to ease the creation of standalone Common Lisp applications by
providing a powerful and uniform command-line option interface.
The most important features of Clon are the following.
- [from the programmer's point of view] Centralized command-line options
  specification and management, including automatic generation of help
  strings, conversion from command-line / environment strings to
  application-level option values, global or on-demand option retrieval, and
  extensibility (the programmer can define his own option types).
- [from the end-user's point of view] Uniform command-line option syntax
  across Clon applications, customization of the help strings layout
  (with optional ISO6429 coloring on terminals that support it),
  possibly abbreviated option calls and short/long syntax."
  :author "Didier Verna <didier@didierverna.net>"
  :maintainer "Didier Verna <didier@didierverna.net>"
  :license "BSD"
  :version #.(com.dvlsoft.clon.setup:version :short)
  :depends-on (:com.dvlsoft.clon.setup
	       :com.dvlsoft.clon.core
	       (:feature :com.dvlsoft.clon.termio :com.dvlsoft.clon.termio)))

;;; com.dvlsoft.clon.asd ends here
