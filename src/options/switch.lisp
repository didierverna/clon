;;; switch.lisp --- Switch options for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:28:03 2008
;; Last Revision: Tue Oct  7 21:29:07 2008

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
;; The Switch Class
;; ============================================================================

;; A switch can appear in the following forms:
;;
;;  -(+)b, --boolean[=yes(no)]          both names, optional argument
;;  -(+)b, --boolean=yes(no)            both names, required argument
;;  -(+)b                               short name, whatever the argument
;;  --boolean[=yes(no)]                 long name,  optional argument
;;  --boolean=yes(no)                   long name,  required argument

;; Switches arguments are optional by default. This is only meaningful for
;; long-name syntax, though, because short names never take an argument (the
;; value is given by the -/+ call. When the argument is optional, omitting it
;; is equivalent to saying yes.

(defoption switch ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :documentation "The option's argument style."
    :initarg :argument-style
    :reader argument-style)
   (argument-styles :documentation "The possible argument styles."
		    :allocation :class
		    :type list
		    :initform '(:yes/no :on/off :true/false :yup/nope)
		    :accessor argument-styles)
   (yes-values :documentation "The possible 'yes' values."
	       :allocation :class
	       :type list
	       :initform '("yes" "on" "true" "yup")
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :allocation :class
	      :type list
	      :initform '("no" "off" "false" "nope")
	      :accessor no-values))
  (:default-initargs
    :argument-type :optional
    :argument-style :yes/no)
  (:documentation "The SWITCH class.
This class implements boolean options."))

(defmethod initialize-instance :before ((switch switch) &key argument-style)
  "Check validity of switch-specific initargs."
  (unless (member argument-style (argument-styles switch))
    (error "Invalid switch argument style ~S." argument-style)))

(defmethod initialize-instance :around
    ((switch switch) &rest keys &key argument-type)
  "Provide a fallback value of t when SWITCH's argument is optional."
  (when (eq argument-type :optional)
    (setq keys (list* :fallback-value t keys)))
  (apply #'call-next-method switch keys))

(defun make-switch (&rest keys &key short-name long-name description env-var
				   argument-type default-value
				   argument-style)
  "Make a new switch.
- SHORT-NAME is the switch's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the switch's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the switch's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the switch's associated environment variable.
  It defaults to nil.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- DEFAULT-VALUE is the switch's default value, if any.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope.
  It defaults to :yes/no."
  (declare (ignore short-name long-name description env-var
		   argument-type default-value
		   argument-style))
  (apply #'make-instance 'switch keys))

(defun make-internal-switch (long-name description
			     &rest keys &key env-var
					    argument-type default-value
					    argument-style)
  "Make a new internal (Clon-specific) switch.
- LONG-NAME is the switch's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the switch's description.
- ENV-VAR is the switch's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- DEFAULT-VALUE is the switch's default value, if any.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope.
  It defaults to :yes/no."
  (declare (ignore env-var argument-type default-value argument-style))
  (apply #'make-instance 'switch
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod option-sticky-distance ((switch switch) namearg)
  "Return 0 (switches don't accept sticky arguments)."
  ;; #### NOTE: see related comment in the FLAG method.
  0)


;; -------------------
;; Char packs protocol
;; -------------------

(defmethod minus-pack-char ((switch switch) &optional as-string)
  "Return SWITCH's minus pack character, if any."
  ;; Here, we don't need to look into the argument type (required or optional)
  ;; as for other options, because for switches, the argument type only has an
  ;;impact on long calls.
  (potential-pack-char switch as-string))

(defmethod plus-pack-char ((switch switch) &optional as-string)
  "Return SWITCH's plus pack character, if any."
  (potential-pack-char switch as-string))


;; -------------------
;; Conversion protocol
;; -------------------

(defmethod check-value ((switch switch) value)
  "Check that VALUE is valid for SWITCH."
  ;; All values are valid for switches: everything but nil means 'yes'."
  value)

(defmethod convert ((switch switch) argument)
  "Convert ARGUMENT to SWITCH's value.
If ARGUMENT is not valid for a switch, raise a conversion error."
  (cond ((member argument (yes-values switch) :test #'string=)
	 t)
	((member argument (no-values switch) :test #'string=)
	 nil)
	(t
	 (error 'invalid-argument
		:option switch
		:argument argument
		:comment
		(concatenate 'string
		  "Valid arguments are: "
		  (reduce (lambda (str1 str2)
			    (concatenate 'string str1 ", " str2))
			  (append (yes-values switch) (no-values switch)))
		  ".")))))


;;; switch.lisp ends here
