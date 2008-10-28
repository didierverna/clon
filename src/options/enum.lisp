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

(defun make-enum (&rest keys
		  &key short-name long-name description
		       argument-name argument-type
		       enum fallback-value default-value
		       env-var)
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
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil."
  (declare (ignore short-name long-name description
		  argument-name argument-type
		  enum fallback-value default-value
		  env-var))
  (apply #'make-instance 'enum keys))

(defun make-internal-enum (long-name description
			    &rest keys
			    &key env-var argument-name argument-type
				 enum fallback-value default-value)
  "Make a new internal (Clon-specific) enum option.
- LONG-NAME is the option's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENUM is the set of possible values.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- ENV-VAR is the option's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil."
  (declare (ignore argument-name argument-type
		   enum fallback-value default-value
		   env-var))
  (apply #'make-instance 'enum
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;; -------------------
;; Conversion protocol
;; -------------------

(defun enum-to-string (enum)
  "Return a coma-separated list of ENUM values."
  (reduce (lambda (name1 name2)
	    (concatenate 'string name1 ", " name2))
	  enum
	  :key #'symbol-name))

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
	   :comment (concatenate 'string
		      "Valid values are: "
		      (enum-to-string (enum enum))
		      ".")))
  value)

(defmethod convert ((enum enum) argument)
  "Convert ARGUMENT to ENUM's value.
ARGUMENT must be a possibly abbreviated symbol name. In case of an
abbreviation, the closest matching symbol is used."
  (setq argument (string-upcase argument))
  (loop :with len = (length argument)
	:with distance = most-positive-fixnum
	:with closest
	:for value in (enum enum)
	:for value-name = (symbol-name value)
	:when (and (beginning-of-string-p argument value-name)
		   (< (- (length value-name) len) distance))
	:do (setq distance (- (length value-name) len)
		  closest value)
	:finally (if closest
		     (return closest)
		     (error 'invalid-argument
			    :option enum
			    :argument argument
			    :comment (concatenate 'string
				       "Valid arguments are: "
				       (enum-to-string (enum enum))
				       ".")))))


;;; enum.lisp ends here
