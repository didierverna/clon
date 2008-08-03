;;; package.lisp --- Package definition for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jun 18 08:49:39 2008
;; Last Revision: Wed Jun 18 08:49:39 2008

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
  (:import-from :clon-system :+version+)
  (:export :+version+
	   :add-to :seal
	   :make-text
	   :make-flag :make-switch :make-stropt
	   :make-group :define-group :declare-group
	   :make-synopsis :define-synopsis
	   :make-context
	   :getopt
	   :getopt-cmdline :do-cmdline-options
	   :getopt-unknown :do-unknown-options))

;;; package.lisp ends here
