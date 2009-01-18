;;; package.lisp --- Package definition for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jun 18 08:49:39 2008
;; Last Revision: Wed Dec 17 16:45:07 2008

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

(in-package :cl-user)

(defpackage :clon
  (:use :cl)
  (:shadow :*readtable*)
  (:import-from :clon-asdf
    :+release-major-level+
    :+release-minor-level+
    :+release-status+ :+release-status-level+
    :+release-name+
    :version)
  (:export :+release-major-level+
	   :+release-minor-level+
	   :+release-status+ :+release-status-level+
	   :+release-name+
	   :version
	   :make-text
	   :make-flag
	   :make-switch
	   :make-stropt :make-enum :make-xswitch :make-lispobj :make-path
	   :make-group :defgroup
	   :make-synopsis :defsynopsis
	   :make-context
	   :getopt
	   :getopt-cmdline :multiple-value-getopt-cmdline :do-cmdline-options
	   :help))


(in-package :clon)

(defvar *readtable* (copy-readtable nil)
  "The Clon readtable.")

(defun tilde-reader (stream char)
  "Read a series of ~\"strings\" to be concatenated together."
  (declare (ignore char))
  (apply #'concatenate 'string
	 (loop :for str = (read stream t nil t)
	       :then (progn (read-char stream t nil t)
			    (read stream t nil t))
	       :collect str
	       :while (eql (peek-char t stream nil nil t) #\~))))

(set-macro-character #\~ #'tilde-reader nil *readtable*)

(defmacro in-readtable (name)
  "Set the current readtable to the value of NAME::*READTABLE*."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (setf cl:*readtable* (symbol-value (find-symbol "*READTABLE*" ,name)))))


;;; package.lisp ends here
