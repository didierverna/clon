;;; switch.lisp --- Switch options

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
;; The Switch Class
;; ==========================================================================

(defoption switch (switch-base)
  ()
  (:documentation "The SWITCH class.
This class implements boolean options."))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((switch switch) value)
  "Transform SWITCH's VALUE into an argument."
  (let ((position (position (argument-style switch)
			    (argument-styles switch))))
    (nth position
	 (if value
	     (yes-values switch)
	   (no-values switch)))))


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((switch switch) value)
  "Check that VALUE is valid for SWITCH."
  (unless (member value '(t nil))
    (error 'invalid-value
	   :option switch
	   :value value
	   :comment "Valid values are T or NIL."))
  value)


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((switch switch) argument)
  "Convert ARGUMENT to a SWITCH value."
  (let ((match (closest-match argument
			      (append (yes-values switch) (no-values switch))
			      :ignore-case t)))
    (cond ((member match (yes-values switch) :test #'string-equal)
	   t)
	  ((member match (no-values switch) :test #'string-equal)
	   nil)
	  (t
	   (error 'invalid-argument
		  :option switch
		  :argument argument
		  :comment (format nil "Valid arguments are: ~A."
			     (list-to-string (append (yes-values switch)
						     (no-values switch)))))))))



;; ==========================================================================
;; Switch Instance Creation
;; ==========================================================================

(defmethod initialize-instance :after ((switch switch) &key)
  "Provide an argument name conformant to the selected argument style."
  (setf (slot-value switch 'argument-name)
	(string-downcase (symbol-name (argument-style switch)))))

(defun make-switch (&rest keys &key short-name long-name description
				   argument-style argument-type
				   env-var default-value hidden)
  "Make a new switch.
- SHORT-NAME is the switch's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the switch's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the switch's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope or :yeah/nah.
  It defaults to :yes/no.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the switch's associated environment variable.
  It defaults to nil.
- DEFAULT-VALUE is the switch's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-style argument-type
		   env-var default-value hidden))
  (apply #'make-instance 'switch keys))

(defun make-internal-switch (long-name description
			     &rest keys &key argument-style argument-type
					    env-var default-value hidden)
  "Make a new internal (Clon-specific) switch.
- LONG-NAME is the switch's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the switch's description.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope or :yeah/nah.
  It defaults to :yes/no.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the switch's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- DEFAULT-VALUE is the switch's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore  argument-style argument-type env-var default-value hidden))
  (apply #'make-instance 'switch
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; switch.lisp ends here
