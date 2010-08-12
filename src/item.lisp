;;; item.lisp --- Item objects

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Nov  2 22:10:17 2008
;; Last Revision: Thu Aug 12 11:29:31 2010

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
;; The Item Class
;; ==========================================================================

(defabstract item ()
  ((traversedp :documentation "The item's traversal state."
	       :initform nil
	       :accessor traversedp)
   (hiddenp :documentation "Whether the item is hidden in help strings."
	    :initform nil
	    ;; #### NOTE: the initarg below doesn't follow the *p convention
	    ;; because that would look strange in a declarative group
	    ;; construction.
	    :initarg :hidden
	    :reader hiddenp))
  (:documentation "The ITEM class.
This class is the base class for all synopsis items."))



;; ==========================================================================
;; The Traversal Protocol
;; ==========================================================================

(defgeneric untraverse (item)
  (:documentation "Reset ITEM's traversal state, and return ITEM.")
  (:method :after ((item item))
    "Mark ITEM as untraversed."
    (setf (traversedp item) nil)))



;; ==========================================================================
;; The Help Specification Protocol
;; ==========================================================================

(defgeneric help-spec (item &key &allow-other-keys)
  (:documentation "Return ITEM's help specification.")
  (:method :around ((item item) &key unhide)
    "Call the actual method only when ITEM is not hidden or UNHIDE."
    (when (or (not (hiddenp item)) unhide)
      (call-next-method))))


;;; item.lisp ends here
