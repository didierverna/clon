;;; net.didierverna.clon.core.asd --- ASDF system definition, core library

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

(defsystem :net.didierverna.clon.core
  :long-name "The Command-Line Options Nuker, core library"
  :description "Clon's basic, platform-independent functionality"
  :long-description "\
Clon's core library provides the platform/feature independent part. For a more
complete description of Clon, see the net.didierverna.clon system."
  :author "Didier Verna"
  :mailto "didier@didierverna.net"
  :homepage "http://www.lrde.epita.fr/~didier/software/lisp/clon.php"
  :source-control "https://github.com/didierverna/clon"
  :license "BSD"
  :version (:read-file-line #p"../make/version.make"
	     :at (1 (lambda (str) (subseq str 19))))
  :depends-on ((:feature :sbcl (:require :sb-posix))
	       :net.didierverna.clon.setup)
  :serial t
  :components ((:file "package")
	       (:module "src"
		:components ((:file "util")
			     (:file "item" :depends-on ("util"))
			     (:file "text" :depends-on ("item"))
			     (:module "options"
			      :depends-on ("text")
			      :components ((:file "option")
					   (:file "flag"
					    :depends-on ("option"))
					   (:file "valued"
					    :depends-on ("option"))
					   (:file "negatable"
					    :depends-on ("valued"))
					   (:file "switch-base"
					    :depends-on ("negatable"))
					   (:file "switch"
					    :depends-on
						  ("switch-base"))
					   (:file "stropt"
					    :depends-on ("valued"))
					   (:file "lispobj"
					    :depends-on ("valued"))
					   (:file "path"
					    :depends-on ("valued"))
					   (:file "enum-base")
					   (:file "enum"
					    :depends-on
						  ("valued"
						   "enum-base"))
					   (:file
					    "xswitch"
					    :depends-on ("valued"
							 "switch-base"
							 "enum-base"))))
			     (:file "container" :depends-on ("options"))
			     (:file "group" :depends-on ("container"))
			     (:module "retrieval"
			      :depends-on ("options")
			      :components ((:file "cmdline")
					   (:file "environ")))
			     (:file "synopsis" :depends-on ("group"))
			     (:module "output"
			      :depends-on ("synopsis" "retrieval")
			      :components ((:file "face")
					   (:file "sheet"
					    :depends-on ("face"))))
			     (:file "context"
			      :depends-on ("output"))))))

;;; net.didierverna.clon.core.asd ends here
