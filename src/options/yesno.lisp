;;; yesno.lisp --- Support for boolean option values

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Nov 26 14:27:23 2008
;; Last Revision: Wed Nov 26 14:27:23 2008

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


;; ==========================================================================
;; The Yes/No Abstract Class
;; ==========================================================================

(defabstract yes/no (plus-callable)
  ((yes-values :documentation "The possible 'yes' values."
	       :allocation :class
	       :type list
	       :initform '("yes" "on" "true" "yup")
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :allocation :class
	      :type list
	      :initform '("no" "off" "false" "nope")
	      :accessor no-values))
  (:documentation "The YES/NO abstract class.
This class provides support for options including boolean values."))

;; #### FIXME: callers of this method will catch an ECOND error with a simple
;; handler-case, which is not satisfactory: this method should trigger a
;; /specific/ conversion error. This suggests to implement an invalid-argument
;; error not related to an option, and build the current invalid-argument
;; error on top of that, renaming it to invliad-option-argument.
(defmethod convert ((yes/no yes/no) argument)
  "Convert (possibly abbreviated) ARGUMENT to t or nil.
ARGUMENT must be a valid YES or NO-VALUE."
  (let ((match (closest-match argument
			      (append (yes-values yes/no) (no-values yes/no))
			      :ignore-case t)))
    (econd ((member match (yes-values yes/no) :test #'string-equal)
	    t)
	   ((member match (no-values yes/no) :test #'string-equal)
	    nil))))


;;; yesno.lisp ends here
