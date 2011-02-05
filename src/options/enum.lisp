;;; enum.lisp --- Enumeration Options

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
;; The Enum Option Class
;; ==========================================================================

(defoption enum (enum-base)
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "TYPE"))
  (:documentation "The ENUM class.
This class implements options whose values belong to a set of keywords."))


;; ------------------------------
;; Value Stringification protocol
;; ------------------------------

(defmethod stringify ((enum enum) value)
  "Transform ENUM's VALUE into an argument."
  #+ecl (declare (ignore enum))
  (string-downcase (symbol-name value)))


;; --------------------
;; Value Check protocol
;; --------------------

(defmethod check ((enum enum) value)
  "Check that VALUE is valid for ENUM."
  (unless (member value (enum enum))
    (error 'invalid-value
	   :option enum
	   :value value
	   :comment (format nil "Valid values are: ~A."
		      (list-to-string (enum enum)
				      :key #'prin1-to-string))))
  value)


;; ----------------------------
;; Argument Conversion protocol
;; ----------------------------

(defmethod convert ((enum enum) argument)
  "Convert ARGUMENT to an ENUM value."
  (or (closest-match argument (enum enum) :ignore-case t :key #'symbol-name)
      (error 'invalid-argument
	     :option enum
	     :argument argument
	     :comment (format nil "Valid arguments are: ~A."
			(list-to-string (enum enum)
					:key (lambda (value)
					       (stringify enum value)))))))



;; ==========================================================================
;; Enum Instance Creation
;; ==========================================================================

(defun make-enum (&rest keys
		  &key short-name long-name description
		       argument-name argument-type
		       enum env-var fallback-value default-value
		       hidden)
  "Make a new enum option.
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
- ENUM is the set of possible values.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		  argument-name argument-type
		  enum env-var fallback-value default-value
		  hidden))
  (apply #'make-instance 'enum keys))

(defun make-internal-enum (long-name description
			    &rest keys
			    &key argument-name argument-type
				 enum env-var fallback-value default-value
				 hidden)
  "Make a new internal (Clon-specific) enum option.
- LONG-NAME is the option's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENUM is the set of possible values.
- ENV-VAR is the option's associated environment variable, sans the 'CLON_'
  prefix. It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   enum env-var fallback-value default-value
		   hidden))
  (apply #'make-instance 'enum
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; enum.lisp ends here
