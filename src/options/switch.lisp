;;; switch.lisp --- Switch options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:28:03 2008
;; Last Revision: Sat Jun 12 18:25:13 2010

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
;; The Switch Class
;; ==========================================================================

;; A switch can appear in the following forms:
;;
;;  -(+)b, --boolean[=yes(no)]          both names, optional argument
;;  -(+)b, --boolean=yes(no)            both names, required argument
;;  -(+)b                               short name, whatever the argument
;;  --boolean[=yes(no)]                 long name,  optional argument
;;  --boolean=yes(no)                   long name,  required argument

;; Switches arguments are optional by default. When the argument is optional,
;; omitting it is equivalent to saying yes.

(defoption switch (switch-base)
  ((argument-name ;; inherited from the VALUED-OPTION class
    :documentation "The option's argument style."
    :initarg :argument-style
    :reader argument-style)
   (nullablep ;; inherited from the VALUED-OPTION class
    :initform t)
   (argument-styles :documentation "The possible argument styles."
		    :allocation :class
		    :type list
		    :initform '(:yes/no :on/off :true/false :yup/nope
				:yeah/nah)
		    :accessor argument-styles))
  (:default-initargs
    :argument-type :optional
    :argument-style :yes/no)
  (:documentation "The SWITCH class.
This class implements boolean options."))


;; -------------------
;; Conversion protocol
;; -------------------

;; Value check subprotocol
(defmethod check-value ((switch switch) value)
  "Check that VALUE is valid for SWITCH."
  (unless (member value '(t nil))
    (error 'invalid-value
	   :option switch
	   :value value
	   :comment "Valid values are t or nil."))
  value)

(defmethod convert ((switch switch) argument)
  "Convert (possibly abbreviated) ARGUMENT to SWITCH's value.
If ARGUMENT is not valid for a switch, raise a conversion error."
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

(defmethod initialize-instance :before ((switch switch) &key argument-style)
  "Check validity of switch-specific initargs."
  (unless (member argument-style (argument-styles switch))
    (error "Invalid switch argument style ~S." argument-style)))

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
