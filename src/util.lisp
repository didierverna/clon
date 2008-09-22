;;; util.lisp --- General utilities for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Jun 30 17:23:36 2008
;; Last Revision: Mon Jun 30 17:23:36 2008

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


;; ============================================================================
;; Misc auxiliary routines
;; ============================================================================

(defmacro endpush (object place)
  "Like push, but at the end."
  `(setf ,place (nconc ,place (list ,object))))

(defun beginning-of-string-p (beginning string)
  "Check that STRING starts with BEGINNING."
  (let ((length (length beginning)))
    (and (>= (length string) length)
	 (string= beginning string :end2 length))))


;; ============================================================================
;; Key-Value pairs manipulation
;; ============================================================================

(defun select-keys (keys &rest selected)
  "Return a new property list from KEYS with only SELECTED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:when (member key selected)
	:nconc (list key val)))

(defun remove-keys (keys &rest removed)
  "Return a new property list from KEYS without REMOVED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:unless (member key removed)
	:nconc (list key val)))


;; ============================================================================
;; CLOS utility routines
;; ============================================================================

(defclass abstract-class (standard-class)
  ()
  (:documentation "The ABSTRACT-CLASS class.
This is the meta-class for abstract classes."))

(defmacro defabstract (class super-classes slots &rest options)
  "Like DEFCLASS, but define an abstract class."
  (when (assoc :metaclass options)
    (error "Defining abstract class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
    (:metaclass abstract-class)))

(defmethod make-instance ((class abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "Instanciating class ~S: is abstract." (class-name class)))

;; #### PORTME.
(defmethod sb-mop:validate-superclass
    ((class abstract-class) (superclass standard-class))
  t)

;; #### PORTME.
(defmethod sb-mop:validate-superclass
    ((class standard-class) (superclass abstract-class))
  t)

#|
(defmacro with-method (method-declaration &body body)
  "Execute BODY with a temporary method defined by METHOD-DECLARATION."
  (let ((method (gensym "method")))
    `(let ((,method (defmethod ,@method-declaration)))
      ,@body
      (remove-method (function ,(car method-declaration)) ,method))))
|#

;;; util.lisp ends here
