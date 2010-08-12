;;; text.lisp --- Text management

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 13:49:48 2008
;; Last Revision: Sat Jun 12 18:20:34 2010

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
;; The Text Class
;; ==========================================================================

(defclass text (item)
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


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((text text) &key)
  "Return TEXT's help specification."
  (accumulate (text)
    (contents text)))



;; ==========================================================================
;; Text Instance Creation
;; ==========================================================================

(defun make-text (&rest keys &key contents hidden)
  "Make a new text.
- CONTENTS is the actual text to display.
- When HIDDEN, the text doesn't appear in help strings."
  (declare (ignore contents hidden))
  (apply #'make-instance 'text keys))

;; #### NOTE: currently, there is no difference between internal and external
;; text, but this might change in the future. Besides, having this function it
;; simplifies the defgroup and defsynopsis macros.
(defun make-internal-text (&rest keys &key contents hidden)
  (declare (ignore contents hidden))
  (apply #'make-text keys))

;;; text.lisp ends here
