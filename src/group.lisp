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
  ()
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

(defun make-group ()
  "Make a new group."
  (make-instance 'group))

(defmacro define-group (group &body body)
  "Evaluate BODY with GROUP bound to a new group, seal it and return it."
  `(let ((,group (make-group)))
    ,@body
    (seal ,group)
    ,group))

(defmacro declare-group (&body forms)
  "Define a new group, add FORMS to it, seal it and return it.
FORMS should be a list of shortcut expressions matching calls to make-group,
make-text, or make-<option> (<option> being an option class, either a Clon
built-in one, or one defined with DEFOPTION), only with the 'make-' prefix
omitted. Each resulting group, text or option created will be automatically
added to the group."
  (let* ((grp (gensym "grp"))
	 (forms (mapcar
		 (lambda (form)
		   (list (intern "ADD-TO" 'clon) grp form))
		 forms))
	 (group (intern "GROUP"))
	 (text (intern "TEXT"))
	 (flag (intern "FLAG")))
    `(macrolet ((,group (&rest args) `(declare-group ,@args))
		(,text (&rest args) `(make-text ,@args))
		(,flag (&rest args) `(make-flag ,@args))
		,@(mapcar
		   (lambda (name)
		     (let ((macro-name (intern name))
			   (make-name (intern (concatenate 'string "MAKE-" name)
					      'clon)))
		       `(,macro-name (&rest args) `(,',make-name ,@args))))
		   *valued-option-names*))
      (define-group ,grp
	,@forms))))


;;; group.lisp ends here
