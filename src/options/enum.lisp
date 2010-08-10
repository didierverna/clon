;;; enum.lisp --- Enumeration Options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct 28 13:50:08 2008
;; Last Revision: Sat Jun 12 18:22:17 2010

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
;; The Enum Option Class
;; ==========================================================================

(defoption enum (enum-base)
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "TYPE"))
  (:documentation "The ENUM class.
This class implements options whose values belong to a set of keywords."))


;; -------------------
;; Conversion protocol
;; -------------------

;; Value check subprotocol
(defmethod check-value ((enum enum) value)
  "Check that VALUE is a valid ENUM."
  (unless (keywordp value)
    (error 'invalid-value
	   :option enum
	   :value value
	   :comment "Value must be a keyword."))
  (unless (member value (enum enum))
    (error 'invalid-value
	   :option enum
	   :value value
	   :comment (format nil "Valid values are: ~A."
		      (symbols-to-string (enum enum)))))
  value)

;; #### FIXME: this is probably wrong because nullable options are not
;; handled. I need to check that, and all other option types as well. In fact,
;; I think I need an :around method in CONVERT that takes the empty string and
;; converts it to NIL is the option is nullable.
(defmethod convert ((enum enum) argument)
  "Convert (possibly abbreviated) ARGUMENT to ENUM's value.
If ARGUMENT doesn't name one of ENUM's symbols, raise a conversion error."
  (or (closest-match argument (enum enum) :ignore-case t :key #'symbol-name)
      (error 'invalid-argument
	     :option enum
	     :argument argument
	     :comment (format nil "Valid arguments are: ~A."
			(symbols-to-string (enum enum))))))



;; ==========================================================================
;; Enum Instance Creation
;; ==========================================================================

(defun make-enum (&rest keys
		  &key short-name long-name description
		       argument-name argument-type
		       enum env-var fallback-value default-value
		       nullablep hidden)
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
- NULLABLEP indicates whether this option accepts nil as a value.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		  argument-name argument-type
		  enum env-var fallback-value default-value
		  nullablep hidden))
  (apply #'make-instance 'enum keys))

(defun make-internal-enum (long-name description
			    &rest keys
			    &key argument-name argument-type
				 enum env-var fallback-value default-value
				 nullablep hidden)
  "Make a new internal (Clon-specific) enum option.
- LONG-NAME is the option's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENUM is the set of possible values.
- ENV-VAR is the option's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- NULLABLEP indicates whether this option accepts nil as a value.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   enum env-var fallback-value default-value
		   nullablep hidden))
  (apply #'make-instance 'enum
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; enum.lisp ends here
