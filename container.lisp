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

;; #### FIXME: make mixin
(defclass container ()
  ((items :documentation "The items in the container."
	  :type list
	  :accessor container-items
	  :initform nil)
   (sealed :documentation "Whether the container is sealed."
	   :accessor container-sealed
	   :initform nil))
  (:documentation "The CONTAINER class.
This class is a mixin used in contexts and groups to represent the program's
command-line hierarchy."))



;; ============================================================================
;; The sealing protocol
;; ============================================================================

;; #### FIXME: see about making this directly the accessor
(defgeneric sealedp (object)
  (:documentation "Returns t if OBJECT is sealed.")
  ;; This function is supposed to work even on non-container objects (options
  ;; and strings) because of the add-to function below, hence the following
  ;; non-specialized default method:
  (:method (object)
    "Return t (consider non-container OBJECTs as sealed)."
    t)
  (:method ((container container))
    "Return t if CONTAINER is sealed."
    (container-sealed container)))

;; On the contrary, this function is not supposed to work on non-container
;; objects, because sealing is manual (so you're supposed to know what you're
;; doing).
(defgeneric seal (container)
  (:documentation "Seal CONTAINER.")
  ;; Common work (checking and marking) is provided below by before: and
  ;; after: methods. However, it's the mixing class's responsibility to
  ;; provide a primary method, empty as it may.
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
  (:documentation "Add ITEM to CONTAINER.")
  ;; There is currently no need to further specialize this function, as
  ;;everything is done below.
  (:method :before ((container container) item)
    "Ensure that ITEM is sealed and CONTAINER is not."
    (when (sealedp container)
      (error "Adding item ~A to container ~A: container sealed." item container))
    (unless (sealedp item)
      (error "Adding item ~A to container ~A: item not sealed." item
	     container)))
  (:method ((container container) item)
    "Add ITEM to CONTAINER."
    (endpush item (container-items container))))


;;; container.lisp ends here
