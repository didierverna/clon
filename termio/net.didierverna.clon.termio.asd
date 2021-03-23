;;; net.didierverna.clon.termio.asd --- ASDF system definition, termio feature

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

(load-system :net.didierverna.clon.setup)

(defsystem :net.didierverna.clon.termio
  :long-name "The Command-Line Options Nuker, termio library"
  :description "Clon's support for termio (tty geometry and fontification)"
  :long-description "\
Clon's termio library provides automatic detection of tty geometry and ISO6429
coloring on terminals that support it. For a more complete description of
Clon, see the net.didierverna.clon system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/clon.php"
  :source-control "https://github.com/didierverna/clon"
  :license "BSD"
  :version #.(net.didierverna.clon.setup:version :short)
  :defsystem-depends-on
  (:net.didierverna.clon.setup/termio
   (:feature :sbcl (:require :sb-grovel))
   (:feature (:or :allegro :clisp :lispworks) :cffi-grovel))
  :if-feature :net.didierverna.clon.termio
  :depends-on ((:feature :sbcl :sb-posix)
	       (:feature (:and :clisp :net.didierverna.clon.termio) :cffi)
	       :net.didierverna.clon.setup :net.didierverna.clon.core)
  :serial t
  :components (#+sbcl
	       (sb-grovel:grovel-constants-file "sbcl/constants"
		 :package :net.didierverna.clon)
	       ;; Cf. https://gitlab.common-lisp.net/asdf/asdf/-/issues/63
	       #+(or allegro clisp lispworks)
	       (:cffi-grovel-file "cffi/constants")
	       (:file "termio")))

;;; net.didierverna.clon.termio.asd ends here
