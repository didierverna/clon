;;; xswitch.lisp --- Extended Switch options for CLon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Oct 30 18:36:30 2008
;; Last Revision: Wed Nov  5 10:26:49 2008

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

(in-package :clon)
(in-readtable :clon)


;; ==========================================================================
;; The Extended Switch Class
;; ==========================================================================

(defoption xswitch (switch-base enum-base)
  ((nullablep ;; inherited from the VALUED-OPTION class
    :initform t)
   (enum ;; inherited from the ENUM-BASE class
    :documentation "The set of possible non-boolean values."))
  (:default-initargs
      :argument-type :optional)
  (:documentation "The XSWITCH class.
This class merges the functionalities of switches and enumerations.
As such, the plus-syntax is available for extended xswitches."))


;; -------------------
;; Conversion protocol
;; -------------------

;; Value check subprotocol
(defmethod check-value ((xswitch xswitch) value)
  "Check that VALUE is valid for XSWITCH."
  ;; All values are valid for xswitches: everything but nil means 'yes'."
  (unless (member value '(t nil))
    (unless (keywordp value)
      (error 'invalid-value
	     :option xswitch
	     :value value
	     :comment "Value must be t, nil or a keyword."))
    (unless (member value (enum xswitch))
      (error 'invalid-value
	     :option xswitch
	     :value value
	     :comment (format nil "Valid values are: t, nil, ~A."
			(symbols-to-string (enum xswitch))))))
  value)

(defmethod convert ((xswitch xswitch) argument)
  "Convert (possibly abbreviated) ARGUMENT to XSWITCH's value.
If ARGUMENT is not valid for an xswitch, raise a conversion error."
  (let* ((other-values (mapcar #'symbol-to-string (enum xswitch)))
	 (all-values (append (yes-values xswitch)
			     (no-values xswitch)
			     other-values))
	 (match (closest-match argument all-values :ignore-case t)))
    (cond ((member match (yes-values xswitch) :test #'string-equal)
	   t)
	  ((member match (no-values xswitch) :test #'string-equal)
	   nil)
	  ((member match other-values :test #'string-equal)
	   (intern (string-upcase match) "KEYWORD"))
	  (t
	   (error 'invalid-argument
		  :option xswitch
		  :argument argument
		  :comment (format nil "Valid arguments are: ~A."
			     (list-to-string all-values)))))))



;; ==========================================================================
;; Extended Switch Instance Creation
;; ==========================================================================

(defmethod initialize-instance :before
    ((xswitch xswitch) &rest keys &key enum)
  (declare (ignore keys))
  (unless enum
    (error "XSwitch ~S: empty enum." xswitch)))

(defmethod initialize-instance :around
    ((xswitch xswitch) &rest keys &key argument-type)
  "Provide a fallback value of t when XSWITCH's argument is optional."
  (when (eq argument-type :optional)
    (setq keys (list* :fallback-value t keys)))
  (apply #'call-next-method xswitch keys))

(defun make-xswitch (&rest keys &key short-name long-name description
				    argument-name argument-type
				    enum env-var default-value)
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
- DEFAULT-VALUE is the xswitch's default value, if any."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   enum env-var default-value))
  (apply #'make-instance 'xswitch keys))

(defun make-internal-xswitch (long-name description
			      &rest keys &key argument-name argument-type
					      enum env-var default-value)
  "Make a new internal (Clon-specific) xswitch.
- LONG-NAME is the xswitch's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the xswitch's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENUM is the set of possible non-boolean values.
- ENV-VAR is the xswitch's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- DEFAULT-VALUE is the xswitch's default value, if any."
  (declare (ignore argument-name argument-type enum env-var default-value ))
  (apply #'make-instance 'xswitch
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; xswitch.lisp ends here
