;;; group.lisp --- Group management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Jul  1 15:52:44 2008
;; Last Revision: Wed Nov  5 10:31:20 2008

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
;; The Group class
;; ==========================================================================

(defclass group (container)
  ((title :documentation "The group's title."
	  :initform nil
	  :initarg :title
	  :reader title))
  (:documentation "The GROUP class.
This class groups other groups, options or strings together, effectively
implementing hierarchical program command-line."))


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((group group) &key)
  "Return GROUP's help specification."
  (let ((dpy (call-next-method)))
    (when dpy
      (push 'group dpy))))



;; ==========================================================================
;; Group Instance Creation
;; ==========================================================================

(defun make-group (&rest keys &key title item)
  "Make a new group."
  (declare (ignore title item))
  (apply #'make-instance 'group keys))

(defmacro %defgroup (internalp (&rest keys) &body forms)
  "Define a new group."
  `(make-group ,@keys
    ,@(loop :for form :in forms
	    :nconc (list :item
			 (let ((operation (symbol-name (car form))))
			   (list* (intern
				   (cond ((string= operation "GROUP")
					  "%DEFGROUP")
					 (t
					  (format nil "MAKE-~:[~;INTERNAL-~]~A"
					    internalp operation)))
				   :clon)
				  (if (string= operation "GROUP")
				      (list* internalp (cdr form))
				      (cdr form))))))))

(defmacro defgroup ((&rest keys) &body forms)
  "Define a new group.
KEYS are initargs to MAKE-GROUP (currently, only :title).
Each form in FORMS will be treated as a new :item.
The CAR of each form is the name of the operation to perform: TEXT, GROUP, or
an option class name. The rest are the arguments to the MAKE-<OP> function or
the DEFGROUP macro."
  `(%defgroup nil ,keys ,@forms))


;;; group.lisp ends here
