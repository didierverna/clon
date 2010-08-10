;;; flag.lisp --- Flag options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:22:05 2008
;; Last Revision: Sat Jun 12 18:22:41 2010

;; This file is part of Clon.

;; Clon is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

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

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; ==========================================================================
;; The Flag Class
;; ==========================================================================

;; A flag can appear in the following forms:

;; -f, --flag                           both names
;; -f                                   short name
;; --flag                               long name

(defclass flag (option)
  ()
  (:documentation "The FLAG class.
This class implements options that don't take any argument."))



;; ==========================================================================
;; Flag Instance Creation
;; ==========================================================================

(defun make-flag (&rest keys &key short-name long-name description env-var
				  hidden)
  "Make a new flag.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the flag's associated environment variable.
  It defaults to nil.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description env-var hidden))
  (apply #'make-instance 'flag keys))

(defun make-internal-flag (long-name description
			   &rest keys &key env-var hidden)
  "Make a new internal (Clon-specific) flag.
- LONG-NAME is the flag's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the flag's description.
- ENV-VAR is the flag's associated environment variable, minus the 'CLON_'
  prefix. It default to nil.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore env-var hidden))
  (apply #'make-instance 'flag
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; flag.lisp ends here
