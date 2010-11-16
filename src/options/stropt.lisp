;;; stropt.lisp --- String Options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:29:39 2008
;; Last Revision: Sat Jun 12 18:24:33 2010

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
;; The String Option Class
;; ==========================================================================

(defoption stropt ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "STR"))
  (:documentation "The STROPT class.
This class implements options the values of which are strings."))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((stropt stropt) value)
  "Transform STROPT's VALUE into an argument."
  #+ecl (declare (ignore stropt))
  value)


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((stropt stropt) value)
  "Check that VALUE is valid for STROPT."
  (if (stringp value)
      value
      (error 'invalid-value
	     :option stropt
	     :value value
	     :comment "Value must be a string.")))


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((stropt stropt) argument)
  "Convert ARGUMENT to an STROPT value."
  #+ecl (declare (ignore stropt))
  argument)



;; ==========================================================================
;; Stropt Instance Creation
;; ==========================================================================

(defun make-stropt (&rest keys
		    &key short-name long-name description
			 argument-name argument-type
			 env-var fallback-value default-value
			 hidden)
  "Make a new string option.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   env-var fallback-value default-value
		   hidden))
  (apply #'make-instance 'stropt keys))

(defun make-internal-stropt (long-name description
			      &rest keys
			      &key argument-name argument-type
				   env-var fallback-value default-value
				   hidden)
  "Make a new internal (Clon-specific) string option.
- LONG-NAME is the option's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   env-var fallback-value default-value
		   hidden))
  (apply #'make-instance 'stropt
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; stropt.lisp ends here
