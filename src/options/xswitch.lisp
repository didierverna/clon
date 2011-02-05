;;; xswitch.lisp --- Extended Switch options

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Clon.

;; Redistribution and use in source or binary form, with or without
;; modification, are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.

;; Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; Neither the names of the authors or copyright holders, nor the names of any
;; contributor or organization may be used to endorse or promote products
;; derived from Clon without specific prior written permission.

;; CLON IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.


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


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((xswitch xswitch) value)
  "Transform XSWITCH's VALUE into an argument."
  (if (member value (enum xswitch))
      (string-downcase (symbol-name value))
    (let ((position (position (argument-style xswitch)
			      (argument-styles xswitch))))
      (nth position
	   (if value
	       (yes-values xswitch)
	     (no-values xswitch))))))


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
					  (mapcar (lambda (value)
						    (stringify xswitch value))
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
