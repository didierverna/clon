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
;; The Container Class
;; ============================================================================

;; #### FIXME: make mixin
(defclass container ()
  ((items :documentation "The items in the container."
	  :type list
	  :accessor container-items
	  :initform nil)
   (sealed :documentation "Whether the container is sealed."
	   :accessor container-sealed
	   :initform nil)
   (traversed :documentation "Whether the container has been traversed."
	      :accessor container-traversed
	      :initform nil))
  (:documentation "The CONTAINER class.
This class is a mixin used in contexts and groups to represent the program's
command-line hierarchy."))


;; ============================================================================
;; The Sealing Protocol
;; ============================================================================

;; #### FIXME: see about making this directly the accessor
(defgeneric sealedp (item)
  (:documentation "Returns t if ITEM is sealed.")
  ;; This function is supposed to work even on non-container items (options
  ;; and strings) because of the add-to function below, hence the following
  ;; non-specialized default method:
  (:method (item)
    "Return t (consider non-container ITEMs as sealed)."
    t)
  (:method ((container container))
    "Return t if CONTAINER is sealed."
    (container-sealed container)))

;; On the contrary, this function is not supposed to work on non-container
;; items, because sealing is manual (so you're supposed to know what you're
;; doing).
(defgeneric seal (container)
  (:documentation "Seal CONTAINER.")
  ;; Common work is provided below, but subclasses will likely implement their
  ;; own primary method as well.
  ;; #### FIXME:  specializers of this function need to (call-next-method)
  ;; somewhere, but the exact place is their own choice, so we can't use a
  ;; progn method combination type.
  (:method :before ((container container))
    "Ensure that CONTAINER is not already sealed."
    (when (container-sealed container)
      (error "Sealing container ~A: already sealed." container)))
  (:method ((container container))
    "Ensure that there is no name clash within CONTAINER's options."
    (loop :for items :on (container-items container)
	  :while (cdr items)
	  :do (loop :for item2 in (cdr items)
		    :do (check-name-clash (car items) item2)))
    container)
  (:method :after ((container container))
    "Mark CONTAINER as sealed."
    (setf (container-sealed container) t)))


;; ============================================================================
;; The Addition Protocol
;; ============================================================================

;; #### NOTE: using a generic function here is overkill because no other
;; method is implemented anywhere else.
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
    ;; #### FIXME: efficiency is really not a concern here, but it would be
    ;; better to just push the elements, and then reverse them when sealing
    ;; the container.
    (endpush item (container-items container))))


;; ============================================================================
;; The Name Clash Check Protocol
;; ============================================================================

;; #### NOTE: currently, name clashes are considered on short and long names
;; independently. That is, it is possible to have a short name identical to a
;; long one, although I don't see why you would want to do that.

(defgeneric check-name-clash (item1 item2)
  (:documentation
   "Check for name clash between ITEM1's options and ITEM2's options.")
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
;; The Traversal Protocol
;; ============================================================================

(defgeneric untraverse (item)
  (:documentation "Reset ITEM's traversal state.")
  (:method (item)
    "Do nothing by default."
    (values))
  (:method ((container container))
    "Untraverse CONTAINER's items."
    (dolist (item (container-items container))
      (untraverse item)))
  (:method :after ((container container))
    "Mark CONTAINER as untraversed."
    (setf (container-traversed container) nil)))

(defgeneric next-option (item)
  (:documentation "Return the next untraversed option in ITEM.")
  (:method (item)
    "Return nil (ITEM doesn't contain or is not an option by default."
    nil)
  (:method ((container container))
    "Return the next option in CONTAINER or one of its sub-containers."
    (unless (container-traversed container)
      (dolist (item (container-items container))
	(let ((opt (next-option item)))
	  (when opt
	    (return-from next-option opt))))
      (setf (container-traversed container) t)
      nil)))

(defmacro do-options ((val container) &body body)
  "Execute BODY with VAL bound to every option in CONTAINER."
  (let ((the-container (gensym "container")))
    `(let ((,the-container ,container))
      (loop :initially (untraverse ,the-container)
	:for ,val = (next-option ,the-container)
	:then (next-option ,the-container)
	:while ,val
	:do ,@body))))



;; ============================================================================
;; The Option Searching Protocol
;; ============================================================================

(defgeneric search-option
    (there &rest keys &key short-name long-name partial-name)
  (:documentation "Search for option in THERE.
The search is done with SHORT-NAME, LONG-NAME, or PARTIAL-NAME.
In case of a PARTIAL-NAME search, look for an option the long name of which
begins with it.")
  (:method
      ((container container) &rest keys &key short-name long-name partial-name)
    "Search for option in CONTAINER."
    (declare (ignore short-name long-name partial-name))
    (do-options (option container)
      (when (apply #'option-matches option
		   (select-keys keys :short-name :long-name :partial-name))
	(return-from search-option option)))))

(defgeneric search-sticky-option (there namearg)
  (:documentation "Search for a sticky option in THERE.
NAMEARG is the concatenation of the option's name and its argument.")
  (:method ((container container) namearg)
    "Search for a sticky option in CONTAINER."
    (do-options (option container)
      (when (option-matches-sticky option namearg)
	(return-from search-sticky-option option)))))


;;; container.lisp ends here
