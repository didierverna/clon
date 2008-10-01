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

(defclass container ()
  ((items :documentation "The items in the container."
	  :type list
	  :initform nil
	  :accessor container-items)
   (sealedp :documentation "Whether the container is sealed."
	    :initform nil
	    :accessor sealedp)
   (traversedp :documentation "The container's traversal state."
	       :initform nil
	       :accessor traversedp))
  (:documentation "The CONTAINER class.
This class is a mixin used in synopsis and groups to represent the program's
command-line hierarchy."))


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
  (:method((container container))
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
;; The Traversal Protocol
;; ============================================================================

(defgeneric untraverse (item)
  (:documentation "Reset ITEM's traversal state.")
  (:method (item)
    "Do nothing by default."
    (values))
  (:method ((container container))
    "Reset the traversal state of all CONTAINER items."
    (dolist (item (container-items container))
      (untraverse item)))
  (:method :after ((container container))
    "Mark CONTAINER as untraversed."
    (setf (traversedp container) nil)))

(defgeneric next-option (item)
  (:documentation "Return the next option in a traversal process.")
  (:method (item)
    "Return nil by default (when ITEM doesn't contain or is not an option)."
    nil)
  (:method ((container container))
    "Try to find the next option in a traversal process in CONTAINER."
    (unless (traversedp container)
      (dolist (item (container-items container))
	(let ((opt (next-option item)))
	  (when opt
	    (return-from next-option opt))))
      (setf (traversedp container) t)
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

;; #### NOTE: when partial matching is requested, the first matching option is
;; returned. For instance, if you have --foobar and --foo options in that
;; order, passing --foo will match the option --foo, but passing --fo will
;; match --foobar. This is probably not the best behavior: it would be better
;; to find the option "closest" to the partial match.
;; #### NOTE: partial matches are not allowed on short options. I didn't think
;; this through, but it would probably make things very difficult wrt sticky
;; options for instance. Think about it again at some point.

(defgeneric search-option
    (there &rest keys &key short-name long-name partial-name)
  (:documentation "Search for option in THERE.
The search is done with SHORT-NAME, LONG-NAME, or PARTIAL-NAME.
In case of a PARTIAL-NAME search, look for an option the long name of which
begins with it.
When such an option exists, return wo values:
- the option itself,
- the name used to find the option, possibly completed if partial.")
  (:method
      ((container container) &rest keys &key short-name long-name partial-name)
    "Search for option in CONTAINER."
    (declare (ignore short-name long-name partial-name))
    (do-options (option container)
      (let ((name
	     (apply #'match-option option
		    (select-keys keys :short-name :long-name :partial-name))))
	(when name
	  (return-from search-option (values option name)))))))

;; #### NOTE: when looking for a sticky option, we stop at the first match,
;; even if, for instance, another option would match a longer part of the
;; argument. This is probably not the best behavior: it would be better to
;; find the option "closest" to the sticky match.
(defgeneric search-sticky-option (there namearg)
  (:documentation "Search for a sticky option in THERE, matching NAMEARG.
NAMEARG is the concatenation of the option's short name and its argument.
When such an option exists, return two values:
- the option itself,
- the argument part of NAMEARG.")
  (:method ((container container) namearg)
    "Search for a sticky option in CONTAINER matching NAMEARG."
    (do-options (option container)
      (let ((argument (match-sticky-option option namearg)))
	(when argument
	  (return-from search-sticky-option (values option argument)))))))


;;; container.lisp ends here
