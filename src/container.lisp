;;; container.lisp --- Container management

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 10:02:47 2008
;; Last Revision: Sat Jun 12 18:18:25 2010

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
;; The Container Class
;; ==========================================================================

(defabstract container (traversable)
  ((items :documentation "The items in the container."
	  :type list
	  :initform nil
	  :initarg :items
	  :reader items))
  (:documentation "The CONTAINER class.
This class is a mixin used in synopsis and groups to represent the program's
command-line hierarchy."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((container container))
  "Untraverse all CONTAINER items."
  (dolist (item (items container))
    (untraverse item))
  container)


;; -------------------------
;; Name clash check protocol
;; -------------------------

(defmethod check-name-clash ((container container) item2)
  "Check for name clash between CONTAINER's options and ITEM2's ones."
  (dolist (item1 (items container))
    (check-name-clash item1 item2)))

(defmethod check-name-clash (item1 (container container))
  "Check for name clash between ITEM1's options and CONTAINER's ones."
  (dolist (item2 (items container))
    (check-name-clash item1 item2)))

(defmethod check-name-clash ((container1 container) (container2 container))
  "Check for name clash between CONTAINER1's options and CONTAINER2's ones."
  (dolist (item1 (items container1))
    (dolist (item2 (items container2))
      (check-name-clash item1 item2))))


;; -------------------------
;; Help specifation protocol
;; -------------------------

(defmethod help-spec ((container container) &key)
  "Return CONTAINER's help specification."
  `(,@(mapcar #'help-spec (items container))))



;; ==========================================================================
;; Container Instance Creation
;; ==========================================================================

(defmethod initialize-instance :around
    ((container container) &rest keys &key item)
  "Canonicalize initialization arguments.
This involves:
- computing the :items initarg from the :item ones."
  (declare (ignore item))
  (apply #'call-next-method container
	 :items (remove :item (select-keys keys :item))
	 (remove-keys keys :item)))

(defmethod initialize-instance :after ((container container) &key)
  "Perform name clash check on CONTAINER's items."
  (loop :for items :on (items container)
	:while (cdr items)
	:do (loop :for item2 in (cdr items)
		  :do (check-name-clash (car items) item2))))


;;; container.lisp ends here
