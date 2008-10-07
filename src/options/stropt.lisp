;;; stropt.lisp --- String Options for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:29:39 2008
;; Last Revision: Tue Oct  7 21:30:49 2008

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
;; The String Option Class
;; ============================================================================

;; #### NOTE: all of this applies to user-defined options as well.

;; A string option can appear in the following formats:
;;
;;   -o, --option=STR                   both names, required argument
;;   -o, --option[=STR]                 both names, optional argument
;;   -o STR                             short name, required argument
;;   -o [STR]                           short name, optional argument
;;   --option=STR                       long name,  required argument
;;   --option[=STR]                     long name,  optional argument

;; String option's arguments are required by default. In such a case, you
;; might provide the argument in the next cmdline item after either a short or
;; long name. if the argument is optional, then giving it must be done after
;; an equal sign for long names, or as a sticky argument after a short name,
;; but that's all.

(defoption stropt ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "STR"))
  (:documentation "The STROPT class.
This class implements options the values of which are strings."))

(defun make-stropt (&rest keys
		    &key short-name long-name description env-var
			 argument-name argument-type fallback-value default-value)
  "Make a new string option.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any."
  (declare (ignore short-name long-name description env-var
		   argument-name argument-type fallback-value default-value))
  (apply #'make-instance 'stropt keys))

(defun make-internal-stropt (long-name description
			      &rest keys
			      &key env-var argument-name
				   argument-type fallback-value default-value)
  "Make a new internal (Clon-specific) string option.
- LONG-NAME is the option's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ENV-VAR is the option's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any."
  (declare (ignore env-var
		   argument-name argument-type fallback-value default-value
		   ))
  (apply #'make-instance 'stropt
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;; -------------------
;; Conversion protocol
;; -------------------

(defmethod check-value ((stropt stropt) value)
  "Check that VALUE is valid for STROPT."
  (if (stringp value)
      value
      (error 'invalid-value
	     :option stropt
	     :value value
	     :comment "Value must be a string.")))

(defmethod convert ((stropt stropt) argument)
  "Return ARGUMENT."
  argument)


;;; stropt.lisp ends here
