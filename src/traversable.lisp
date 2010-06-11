;;; traversable.lisp --- Traversable objects for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Nov  2 22:10:17 2008
;; Last Revision: Wed Nov  5 09:26:18 2008

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

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; ==========================================================================
;; The Traversable Class
;; ==========================================================================

(defabstract traversable ()
  ((traversedp :documentation "The object's traversal state."
	       :initform nil
	       :accessor traversedp))
  (:documentation "The TRAVERSABLE class.
This class is used for traversing graphs with loop avoidance."))



;; ==========================================================================
;; The Traversal Protocol
;; ==========================================================================

(defgeneric untraverse (object)
  (:documentation "Reset OBJECT's traversal state, and return OBJECT.")
  (:method :after((traversable traversable))
    "Mark TRAVERSABLE as untraversed."
    (setf (traversedp traversable) nil)))


;;; traversable.lisp ends here
