;;; lispobj.lisp --- Read-from-string options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Thu Nov 27 18:04:15 2008
;; Last Revision: Sat Jun 12 18:23:09 2010

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
;; The LispObj Option Class
;; ==========================================================================

(defoption lispobj ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "OBJ")
   (typespec :documentation "A type specifier the option's value should satisfy."
	     :initform t
	     :initarg :typespec
	     :reader typespec))
  (:documentation "The LISPOBJ class.
This class implements read-from-string options."))


;; -------------------
;; Conversion protocol
;; -------------------

;; Value check subprotocol
(defmethod check-value ((lispobj lispobj) value)
  "Check that VALUE is valid for LISPOBJ."
  (if (typep value (typespec lispobj))
      value
      (error 'invalid-value
	     :option lispobj
	     :value value
	     :comment (format nil "Value must satisfy ~A." (typespec lispobj)))))
;; #### FIXME: I need to handle other errors than just end-of-file. Probably
;; all reader errors, and why not simply all errors.
(defmethod convert ((lispobj lispobj) argument)
  "Return the evaluation of ARGUMENT string."
  (multiple-value-bind (value position)
      (handler-case (read-from-string argument)
	(end-of-file ()
	  (error 'invalid-argument
		 :option lispobj
		 :argument argument
		 :comment (format nil "Cannot parse argument ~S." argument))))
    (cond ((< position (length argument))
	   (error 'invalid-argument
		  :option lispobj
		  :argument argument
		  :comment (format nil "Cannot parse argument ~S." argument)))
	  ((typep value (typespec lispobj))
	   value)
	  (t
	   (error 'invalid-argument
		  :option lispobj
		  :argument argument
		  :comment (format nil "Argument ~S must evaluate to ~A."
			     argument (typespec lispobj)))))))



;; ==========================================================================
;; LispObj Instance Creation
;; ==========================================================================

(defun make-lispobj (&rest keys
		     &key short-name long-name description
			  argument-name argument-type
			  env-var
			  typespec fallback-value default-value
			  nullablep hidden)
  "Make a new lispobj option.
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
- TYPESPEC is a type specifier the option's value should satisfy.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- NULLABLEP indicates whether this option accepts nil as a value.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   env-var
		   typespec fallback-value default-value
		   nullablep hidden))
  (apply #'make-instance 'lispobj keys))

(defun make-internal-lispobj (long-name description
			       &rest keys
			       &key argument-name argument-type
				    env-var
				    typespec fallback-value default-value
				    nullablep hidden)
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
- TYPESPEC is a type specifier the option's value should satisfy.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- NULLABLEP indicates whether this option accepts nil as a value.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   env-var
		   typespec fallback-value default-value
		   nullablep hidden))
  (apply #'make-instance 'lispobj
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; lispobj.lisp ends here
