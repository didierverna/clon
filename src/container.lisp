;;; container.lisp --- Container management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 10:02:47 2008
;; Last Revision: Wed Jul  2 10:02:47 2008

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


;; ============================================================================
;; The Container Class
;; ============================================================================

(defclass container (traversable)
  ((items :documentation "The items in the container."
	  :type list
	  :initform nil
	  :accessor container-items)
   (sealedp :documentation "Whether the container is sealed."
	    :initform nil
	    :accessor sealedp))
  (:documentation "The CONTAINER class.
This class is a mixin used in synopsis and groups to represent the program's
command-line hierarchy."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((container container))
  "Untraverse all CONTAINER items."
  (dolist (item (container-items container))
    (untraverse item))
  container)


;; -------------------------
;; Name clash check protocol
;; -------------------------

(defmethod check-name-clash ((container container) item2)
  "Check for name clash between CONTAINER's options and ITEM2's ones."
  (dolist (item1 (container-items container))
    (check-name-clash item1 item2)))

(defmethod check-name-clash (item1 (container container))
  "Check for name clash between ITEM1's options and CONTAINER's ones."
  (dolist (item2 (container-items container))
    (check-name-clash item1 item2)))

(defmethod check-name-clash ((container1 container) (container2 container))
  "Check for name clash between CONTAINER1's options and CONTAINER2's ones."
  (dolist (item1 (container-items container1))
    (dolist (item2 (container-items container2))
      (check-name-clash item1 item2))))



;; ============================================================================
;; The Sealing Protocol
;; ============================================================================

(defgeneric seal (container)
  (:documentation "Seal CONTAINER.")
  (:method :before ((container container))
    "Ensure that CONTAINER is not already sealed, and perform name clash check."
    (when (sealedp container)
      (error "Sealing container ~A: already sealed." container))
    (loop :for items :on (container-items container)
	  :while (cdr items)
	  :do (loop :for item2 in (cdr items)
		    :do (check-name-clash (car items) item2))))
  (:method ((container container))
    "Mark CONTAINER as sealed."
    (setf (sealedp container) t)))



;; ============================================================================
;; The Addition Protocol
;; ============================================================================

;; #### NOTE: there's actually only one primary method for this function, as
;; defined below. The use of a generic function is a bit overkill, but it
;; allows to specialize the before-method below to perform some sanity checks.
(defgeneric add-to (container item)
  (:documentation "Add ITEM to CONTAINER.")
  (:method :before ((container container) (item container))
    "Ensure that container ITEM is sealed and that CONTAINER is not."
    (when (sealedp container)
      (error "Adding item ~A to container ~A: container sealed."
	     item container))
    (unless (sealedp item)
      (error "Adding item ~A to container ~A: item not sealed."
	     item container)))
  (:method ((container container) item)
    "Add ITEM to CONTAINER."
    (endpush item (container-items container))))


;;; container.lisp ends here
