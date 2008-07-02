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


;; ============================================================================
;; The container class
;; ============================================================================

;; #### FIXME: make final
(defclass container ()
  ((items :documentation "The items in the container."
	  :type list
	  :accessor container-items
	  :initform nil)
   (sealed :documentation "Whether the container is sealed."
	   :accessor container-sealed
	   :initform nil))
  (:documentation "The CONTAINER class.
This class is used as a mixin for Clon contexts and groups, to hold groups,
options and strings."))


;; ============================================================================
;; The sealing protocol
;; ============================================================================

;; This function is supposed to work even on non-container objects (options
;; and strings) because of the add-to function below.
(defgeneric sealedp (object)
  (:documentation "Returns t if OBJECT is sealed.")
  (:method (object)
    "Non container OBJECTs (options and strings) are always sealed."
    t)
  (:method ((container container))
    "Returns t if the CONTAINER is sealed."
    (container-sealed container)))

;; This one, however, is not. That's because sealing is manual, so you're
;; supposed to know what you're doing.
(defgeneric seal (container)
  (:documentation "Seal OBJECT.
After OBJECT is sealed, it is not possible to modify its items.")
  (:method :before ((container container))
    "Ensure that CONTAINER is not already sealed."
    (when (container-sealed container)
      (error "Sealing container ~A: already sealed." container)))
  (:method :after ((container container))
    "Mark CONTAINER as sealed."
    (setf (container-sealed container) t)))


;; ============================================================================
;; The addition protocol
;; ============================================================================

(defgeneric add-to (container item)
  (:documentation "Add ITEM to CONTAINER.
ITEM must be sealed, CONTAINER must not.")
  (:method :before ((container container) item)
    "Ensure that ITEM is sealed and CONTAINER is not."
    (when (sealedp container)
      (error "Adding item ~A to container ~A: object sealed." item container))
    (unless (sealedp item)
      (error "Adding item ~A to container ~A: item not sealed." item
	     container)))
  (:method ((container container) item)
    "Append ITEM to CONTAINER's items."
    (endpush item (container-items container))))



;;; container.lisp ends here
