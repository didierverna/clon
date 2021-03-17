;;; package.lisp --- Clon package definition

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

(in-package :cl-user)


(defpackage :net.didierverna.clon
  (:documentation "The Clon library's package.")
  (:use :cl :net.didierverna.clon.setup)
  ;; #### PORTME.
  (:import-from #+sbcl      :sb-mop
		#+cmu       :mop
		#+ccl       :ccl
		#+ecl       :clos
		#+clisp     :clos
		#+abcl      :mop
		#+allegro   :mop
		#+lispworks :clos
		:class-slots :slot-definition-name :validate-superclass)
  (:export
    ;; From the :net.didierverna.clon.setup package:
    :*release-major-level*
    :*release-minor-level*
    :*release-status*
    :*release-status-level*
    :*release-name*
    :version
    ;; From package.lisp (this file):
    :nickname-package
    ;; From src/util.lisp:
    :exit
    :cmdline
    :dump
    ;; From src/text.lisp:
    :make-text
    ;; From src/options/flag.lisp:
    :make-flag
    ;; From src/options/switch.lisp:
    :make-switch
    ;; From src/options/stropt.lisp:
    :make-stropt
    ;; From src/options/lispobj.lisp:
    :make-lispobj
    ;; From src/options/path.lisp:
    :make-path
    ;; From src/options/enum.lisp:
    :make-enum
    ;; From src/options/xswitch.lisp:
    :make-xswitch
    ;; From src/group.lisp:
    :make-group :defgroup
    ;; From src/synopsis.lisp:
    :*synopsis*
    :make-synopsis :defsynopsis
    ;; From src/context.lisp:
    :*context*
    :make-context
    :with-context
    :progname
    :remainder
    :cmdline-options-p
    :cmdline-p
    :getopt
    :getopt-cmdline
    :multiple-value-getopt-cmdline
    :do-cmdline-options
    :help))


(in-package :net.didierverna.clon)

(defun nickname-package (&optional (nickname :clon))
  "Add NICKNAME (:CLON by default) to the :NET.DIDIERVERNA.CLON package."
  (rename-package :net.didierverna.clon
		  (package-name :net.didierverna.clon)
		  (adjoin nickname (package-nicknames :net.didierverna.clon)
			  :test #'string-equal)))

;;; package.lisp ends here
