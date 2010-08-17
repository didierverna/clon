;;; xswitch.lisp --- Extended Switch options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Oct 30 18:36:30 2008
;; Last Revision: Sat Jun 12 18:26:01 2010

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
;; The Extended Switch Class
;; ==========================================================================

(defoption xswitch (switch-base enum-base)
  ((enum ;; inherited from the ENUM-BASE class
    :documentation "The set of possible non-boolean values."))
  (:documentation "The XSWITCH class.
This class merges the functionalities of switches and enumerations.
As such, the negated syntax is available for extended xswitches."))


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((xswitch xswitch) value)
  "Check that VALUE is valid for XSWITCH."
  (unless (or (member value '(t nil))
	      (member value (enum xswitch)))
    (error 'invalid-value
	   :option xswitch
	   :value value
	   :comment (format nil "Valid values are: T, NIL, ~A."
		      (list-to-string (enum xswitch)
				      :key #'prin1-to-string))))
  value)


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((xswitch xswitch) argument)
  "Convert ARGUMENT to an XSWITCH value."
  (let ((match (closest-match argument
			      (append (yes-values xswitch) (no-values xswitch))
			      :ignore-case t)))
    (cond ((member match (yes-values xswitch) :test #'string-equal)
	   t)
	  ((member match (no-values xswitch) :test #'string-equal)
	   nil)
	  (t
	   (or (closest-match argument (enum xswitch)
			      :ignore-case t :key #'symbol-name)
	       (error 'invalid-argument
		      :option xswitch
		      :argument argument
		      :comment (format nil "Valid arguments are: ~A."
				 (list-to-string
				  (append (yes-values xswitch)
					  (no-values xswitch)
					  (mapcar #'symbol-to-string
						  (enum xswitch)))))))))))



;; ==========================================================================
;; Extended Switch Instance Creation
;; ==========================================================================

(defun make-xswitch (&rest keys &key short-name long-name description
				    argument-name argument-type
				    enum env-var default-value hidden)
  "Make a new xswitch.
- SHORT-NAME is the xswitch's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the xswitch's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the xswitch's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENUM is the set of possible non-boolean values.
- ENV-VAR is the xswitch's associated environment variable.
  It defaults to nil.
- DEFAULT-VALUE is the xswitch's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   enum env-var default-value hidden))
  (apply #'make-instance 'xswitch keys))

(defun make-internal-xswitch (long-name description
			      &rest keys &key argument-name argument-type
					      enum env-var default-value
					      hidden)
  "Make a new internal (Clon-specific) xswitch.
- LONG-NAME is the xswitch's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the xswitch's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENUM is the set of possible non-boolean values.
- ENV-VAR is the xswitch's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- DEFAULT-VALUE is the xswitch's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type enum env-var default-value
		   hidden))
  (apply #'make-instance 'xswitch
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; xswitch.lisp ends here
