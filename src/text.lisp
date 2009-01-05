;;; text.lisp --- Text management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 13:49:48 2008
;; Last Revision: Wed Nov  5 14:30:37 2008

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
;; The Text Class
;; ==========================================================================

(defclass text (traversable)
  ((contents :documentation "The actual text string."
	     :type string
	     :initarg :contents
	     :reader contents))
  (:documentation "The TEXT class.
This class implements plain text objects appearing in a synopsis."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((text text))
  "TEXT is a terminal object: just return it."
  text)



;; ==========================================================================
;; The Help Specification Protocol
;; ==========================================================================

(defgeneric help-spec (item &key &allow-other-keys)
  (:documentation "Return a help specification for ITEM.")
  (:method ((text text) &key)
    `(text ,(contents text))))



;; ==========================================================================
;; Text Instance Creation
;; ==========================================================================

(defun make-text (&rest keys &key contents)
  "Make a new text.
- CONTENTS is the actual text to display."
  (declare (ignore contents))
  (apply #'make-instance 'text keys))

;; #### NOTE: currently, there is no difference between internal and external
;; text, but this might change in the future. Besides, having this function it
;; simplifies the defgroup and defsynopsis macros.
(defun make-internal-text (&rest keys &key contents)
  (declare (ignore contents))
  (apply #'make-text keys))

;;; text.lisp ends here
