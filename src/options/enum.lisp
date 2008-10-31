;;; enum.lisp --- Enumeration Options for CLon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct 28 13:50:08 2008
;; Last Revision: Tue Oct 28 13:50:08 2008

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


;; ============================================================================
;; The Enum Option Class
;; ============================================================================

(defoption enum ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "TYPE")
   (enum :documentation "The set of possible values."
	 :initarg :enum
	 :reader enum))
  (:documentation "The ENUM class.
This class implements options whose values belong to a set of keywords."))

(defmethod initialize-instance :before ((e enum) &rest keys &key enum)
  (declare (ignore keys))
  (unless enum
    (error "Enum ~S: empty enum." e)))

(defun make-enum (&rest keys
		  &key short-name long-name description
		       argument-name argument-type
		       enum env-var fallback-value default-value
		       nullablep)
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
- NULLABLEP indicates whether this option accepts nil as a value."
  (declare (ignore short-name long-name description
		  argument-name argument-type
		  enum env-var fallback-value default-value
		  nullablep))
  (apply #'make-instance 'enum keys))

(defun make-internal-enum (long-name description
			    &rest keys
			    &key argument-name argument-type
				 enum env-var fallback-value default-value
				 nullablep)
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
- NULLABLEP indicates whether this option accepts nil as a value."
  (declare (ignore argument-name argument-type
		   enum env-var fallback-value default-value
		   nullablep))
  (apply #'make-instance 'enum
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;; -------------------
;; Conversion protocol
;; -------------------

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

(defmethod convert ((enum enum) argument)
  "Convert ARGUMENT to ENUM's value.
ARGUMENT must be a possibly abbreviated symbol name. In case of an
abbreviation, the closest matching symbol is used."
  (or (closest-match (string-upcase argument) (enum enum) :key #'symbol-name)
      (error 'invalid-argument
	     :option enum
	     :argument argument
	     :comment (format nil "Valid arguments are: ~A."
			(symbols-to-string (enum enum))))))


;;; enum.lisp ends here
