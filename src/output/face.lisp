;;; face.lisp --- Face management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Dec 24 17:37:38 2008
;; Last Revision: Wed Dec 24 17:37:38 2008

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


;; =========================================================================
;; The Face Class
;; =========================================================================

(defclass face ()
  ((name :documentation "The face name."
	 :initarg :name
	 :reader name)
   ;; Layout properties:
   (display :documentation "The face display mode."
	    :initarg :display
	    :initform :inilne
	    :reader display)
   (left-padding :documentation "The face left padding."
		 :initarg :left-padding
		 :initform 0
		 :reader left-padding)
   (separator :documentation "The face separator."
	      :initarg :separator
	      :initform nil
	      :reader separator)
   (item-separator :documentation "The face item separator."
		   :initarg :item-separator
		   :initform #\space
		   :reader item-separator)
   ;; Tree structure:
   (subfaces :documentation "The face children."
	     :initarg :subfaces
	     :reader subfaces)
   (parent :documentation "The face parent."
	   :initform nil
	   :reader parent))
  (:documentation "The FACE class."))



;; =========================================================================
;; The Face Property Access Protocol
;; =========================================================================

;; #### FIXME: investigate why define-constant doesn't work here.
(defvar *highlight-face-properties* '()
  "The highlight face properties and their default value.")

(defmethod slot-unbound (class (face face) slot)
  "Look up SLOT's value in FACE's parent if it's a highlight property.
Otherwise, trigger an error."
  (let ((property (assoc slot *highlight-face-properties*)))
    (if property
	(if (parent face)
	    (slot-value (parent face) slot)
	    (cadr property))
	(call-next-method))))



;; =========================================================================
;; The Face Tree Copy Protocol
;; =========================================================================

(defun attach-face-tree (face tree)
  "Create a copy of TREE, attach it to FACE and return it."
  (let ((new-tree (copy-instance tree)))
    (setf (slot-value new-tree 'subfaces)
	  (mapcar (lambda (subtree)
		    (attach-face-tree new-tree subtree))
		  (subfaces new-tree)))
    (setf (slot-value new-tree 'parent) face)
    (push new-tree (slot-value face 'subfaces))
    new-tree))

(defun subfacep (name face)
  "Return subface named NAME from FACE, or nil."
  (find name (subfaces face) :key #'name))

;; #### FIXME: we should also look for all subtrees in the ancestor hierarchy.
(defun find-face (name face)
  "Find face named NAME in face FACE.
Face should be either a direct subface of FACE (in which case it is simply
returned) or a subface of one of FACE's parents (in which case the whole face
tree is copied as a new subface of FACE)."
  (or (subfacep name face)
      (loop :for parent := (parent face) :then (parent parent)
	    :while parent
	    :for found := (subfacep name parent)
	    :when found
	    :do (return (attach-face-tree face found))
	    :finally (error "Face ~A not found." name))))

(defun parent-generation (face parent-name)
  "Return FACE's parent generation for PARENT-NAME.
That is, 1 if PARENT-NAME names FACE's parent, 2 if it names its grand-parent
etc. If PARENT-NAME does not name one of FACE's ancestors, trigger an error."
  (loop :for generation :from 1
	:for parent := (parent face) :then (parent parent)
	:while parent
	:when (eql (name parent) parent-name)
	:do (return generation)
	:finally (error "Parent face ~A for face ~A not found."
			parent-name (name face))))



;; =========================================================================
;; Face Instance Creation
;; =========================================================================

;; #### NOTE: although we don't use it explicitely, the SUBFACE initarg is
;; declared valid below.
(defmethod initialize-instance :around ((face face) &rest keys &key subface)
  "Compute :subfaces initarg from the :subface ones."
  (declare (ignore subface))
  (apply #'call-next-method face
	 :subfaces (remove :subface (select-keys keys :subface))
	 (remove-keys keys :subface)))

;; #### NOTE: we use the NAME keyword here because we're in the before method,
;; hence FACE name has not been initialized yet.
(defmethod initialize-instance :before ((face face) &key name subfaces)
  "Check for unicity of FACE subfaces."
  (loop :for faces :on subfaces
	:while (cdr faces)
	:when (member (name (car faces))
		      (mapcar #'name (cdr faces)))
	:do (error "Duplicate subface ~A for face ~A."
		   (name (car faces)) name)))

(defmethod initialize-instance :after ((face face) &key)
  "Fill in the parent slot of all subfaces."
  (mapc (lambda (child)
	  (setf (slot-value child 'parent) face))
	(subfaces face)))

(defun make-face (name
		  &rest keys
		  &key display left-padding separator item-separator subface)
  "Make a new face named NAME."
  (declare (ignore display left-padding separator item-separator subface))
  (apply #'make-instance 'face :name name keys))

;; #### NOTE: face properties are all inherited now, but I'm not sure that's a
;; good idea for all of them, especially the layout ones. Only highlight
;; properties should probably be inherited; not layout ones.
(defun make-face-tree ()
  (make-face 'help
    :display :block
    :item-separator #\newline
    :subface (make-face 'synopsis
	       :display :block
	       :separator #\newline
	       :subface (make-face 'program)
	       :subface (make-face 'minus-pack)
	       :subface (make-face 'plus-pack)
	       :subface (make-face 'options)
	       :subface (make-face 'postfix))
    :subface (make-face 'text :display :block)
    :subface (make-face 'option
	       :display :block
	       :left-padding 2
	       :subface (make-face 'syntax
			  :item-separator ", "
			  :subface (make-face 'short-name
				     :item-separator nil
				     :subface (make-face 'argument))
			  :subface (make-face 'long-name
				     :item-separator nil
				     :subface (make-face 'argument)))
	       :subface (make-face 'description
			  :display :block
			  :left-padding '(30 :absolute)
			  :item-separator #\newline
			  :subface (make-face 'fallback)
			  :subface (make-face 'default)
			  :subface (make-face 'environment)))
    :subface (make-face 'group
	       :display :block
	       :left-padding 2
	       :item-separator #\newline)))


;;; face.lisp ends here
