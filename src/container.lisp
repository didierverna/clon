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


;; #### NOTE: I don't like putting this function here (before the CONTAINER
;; class, outside the sealing protocol section), but this lets me specify
;; directly the :accessor in the class below.
(defgeneric sealedp (item)
  (:documentation "Returns t if ITEM is sealed.")
  ;; This function is supposed to work even on non-container items (options
  ;; and strings) because of the add-to function below, hence the following
  ;; non-specialized default method:
  (:method (item)
    "Return t (consider non-container ITEMs as sealed)."
    t))



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



;; #### TODO: the name clash check and sealing protocol are not well placed
;; here. There's something to do to improve the dependencies.

;; ============================================================================
;; The Name Clash Check Protocol
;; ============================================================================

;; #### NOTE: currently, name clashes are considered on short and long names
;; independently. That is, it is possible to have a short name identical to a
;; long one, although I don't see why you would want to do that, and I should
;; probably prohibit it altogether.

(defgeneric check-name-clash (item1 item2)
  (:documentation ~"Check for name clash between ITEM1's options "
		  ~"and ITEM2's options.")
  (:method (item1 item2)
    "Do nothing (no name clash for a non-group or non-option ITEMs."
    (values))
  (:method ((container container) item2)
    "Check for name clash between CONTAINER's options and ITEM2's ones."
    (dolist (item1 (container-items container))
      (check-name-clash item1 item2)))
  (:method (item1 (container container))
    "Check for name clash between ITEM1's options and CONTAINER's ones."
    (dolist (item2 (container-items container))
      (check-name-clash item1 item2)))
  (:method ((container1 container) (container2 container))
    "Check for name clash between CONTAINER1's options and CONTAINER2's ones."
    (dolist (item1 (container-items container1))
      (dolist (item2 (container-items container2))
	(check-name-clash item1 item2)))))


;; ============================================================================
;; The Sealing Protocol
;; ============================================================================

;; Contrary to SEALEDP, this function is not supposed to work on non-container
;; items, because sealing is manual (so you're supposed to know what you're
;; doing).
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

;; #### NOTE: using a generic function here is overkill because no other
;; method is implemented anywhere else.
(defgeneric add-to (container item)
  (:documentation "Add ITEM to CONTAINER.")
  ;; There is currently no need to further specialize this function, as
  ;; everything is done below.
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
