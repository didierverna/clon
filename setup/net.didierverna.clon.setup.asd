;;; net.didierverna.clon.setup.asd --- ASDF system definition

;; Copyright (C) 2015, 2021 Didier Verna

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

(defsystem :net.didierverna.clon.setup
  :long-name "The Command-Line Options Nuker, setup library"
  :description "Clon's preload setup library"
  :long-description "\
The Clon setup library provides support for various preload configuration
parameters and meta-utilities. For a more complete description of Clon,
see the `net.didierverna.clon' system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/clon.php"
  :source-control "https://github.com/didierverna/clon"
  :license "BSD"
  :version (:read-file-line #p"../make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on (:named-readtables)
  :serial t
  :components ((:file "package")
	       (:module "src"
		:components ((:file "configuration")
			     (:file "readtable" :depends-on ("configuration"))
			     (:file "version" :depends-on ("readtable"))
			     (:file "termio" :depends-on ("readtable"))))))

(defsystem :net.didierverna.clon.setup/termio
  :long-name "The Command-Line Options Nuker, termio setup"
  :description "Clon's support for automatic configuration of termio support"
  :long-description "\
This is a virtual subsystem or Clon (no actual code). Its purpose is only to
autodetect termio support and update Clon's preload configuration on load. For
a more complete description of Clon, see the net.didierverna.clon system."
  :author "Didier Verna <didier@didierverna.net>"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/clon.php"
  :source-control "https://github.com/didierverna/clon"
  :license "BSD"
  :version (:read-file-line #p"../make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on (:net.didierverna.clon.setup)
  :perform (load-op (o c)
	     (declare (ignore o c))
	     (call-function "net.didierverna.clon.setup:setup-termio")))

;;; net.didierverna.clon.setup.asd ends here
