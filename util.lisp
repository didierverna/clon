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
;; Basic auxiliary routines
;; ============================================================================

(defmacro endpush (object place)
  "Like push, but at the end."
  `(setf ,place (nconc ,place (list ,object))))

(defun string-start (string start)
  "Check that STRING starts with START.
This check also protects against too short strings."
  (let ((length (length start)))
    (and (>= (length string) length)
	 (string= string start :end1 length))))


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


;;; util.lisp ends here
