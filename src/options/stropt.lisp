;;; stropt.lisp --- String Options

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
