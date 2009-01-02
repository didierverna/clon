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
   ;; Highlight (ISO/IEC 6429 SGR) properties:
   ;; Note that although we have some boolean slots below, their initargs
   ;; don't follow the usual *p convention. That's because they would look
   ;; strange when used as declarative properties in a theme file, where it is
   ;; not obvious to the end-user that she's actually calling instantiation
   ;; functions with initargs.
   (intensity :documentation "The face intensity."
	      :initarg :intensity
	      :reader intensity)
   (italicp :documentation "The face's italic status."
	    :initarg :italic
	    :reader italicp)
   (underline :documentation "The face's underline level."
	      :initarg :underline
	      :reader underline)
   (blink :documentation "The face's blink speed."
	  :initarg :blink
	  :reader blink)
   (inversep :documentation "The face's inverse video status."
	     :initarg :inverse
	     :reader inversep)
   (concealedp :documentation "The face's concealed status."
	       :initarg :concealed
	       :reader concealedp)
   (crossed-out-p :documentation "The face's crossed out status."
		  :initarg :crossed-out
		  :reader crossed-out-p)
   (framedp :documentation "The face's framed status."
	    :initarg :framed
	    :reader framedp)
   (foreground :documentation "The face foreground."
	       :initarg :foreground
	       :reader foreground)
   (background :documentation "The face background."
	       :initarg :background
	       :reader background)
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
(defvar *highlight-face-properties*
  '(intensity italicp underline blink inversep concealedp crossed-out-p
    framedp foreground background)
  "The highlight face properties.")

(defmethod slot-unbound (class (face face) slot)
  "Look up SLOT's value in FACE's parent if it's a highlight property.
Otherwise, trigger an error."
  (let ((property (member slot *highlight-face-properties*)))
    (if property
	(when (parent face)
	  (slot-value (parent face) slot))
	(call-next-method))))



;; =========================================================================
;; The Face Tree Copy Protocol
;; =========================================================================

(defun attach-face-tree (face face-tree)
  "Create a copy of FACE-TREE, attach it to FACE and return it.
Apart from the parenting information, the copied faces share slot values with
the original ones."
  (let ((new-tree (copy-instance face-tree)))
    (setf (slot-value new-tree 'subfaces)
	  (mapcar (lambda (subtree)
		    (attach-face-tree new-tree subtree))
		  (subfaces new-tree)))
    (setf (slot-value new-tree 'parent) face)
    (push new-tree (slot-value face 'subfaces))
    new-tree))

(defgeneric subface (face |name(s)|)
  (:documentation "Return subface of FACE named NAME(S) or nil.
If a list of names is provided instead of a single one, follow a subface
branch matching those names to find the last one.")
  (:method (face (name symbol))
    "Return FACE'subface named NAME, or nil."
    (find name (subfaces face) :key #'name))
  (:method (face (names list))
    "Return the leaf face from FACE'subbranch matching NAMES, or nil."
    (let ((branch (subface face (car names))))
      (or (when (null (cdr names))
	    branch)
	  (when branch
	    (subface branch (cdr names)))))))

(defun search-face (face names)
  "Search for a face branch named NAMES starting at FACE.
The branch is searched for as a direct subbranch of FACE, or as a direct
subbranch of one of FACE's parents.
Return the leaf face or nil."
  (or (subface face names)
      (loop :for parent := (parent face) :then (parent parent)
	    :while parent
	    :for found := (subface parent names)
	    :when found
	    :return found
	    :finally (return nil))))

(defun find-face (face name)
  "Find a face named NAME starting at FACE.
The face is looked for as a direct subface of FACE (in which case it is simply
returned), or up in the hierarchy and by successive upper branches (in which
case it is copied and attached to FACE).
If no face is found, trigger an error."
  (or (subface face name)
      (loop :with names := (list name)
	    :for child := face :then (parent child)
	    :for parent := (parent child) :then (parent parent)
	    :while parent
	    :for found := (search-face parent names)
	    :if found
	    :return (attach-face-tree face found)
	    :else
	    :do (push (name child) names)
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

;; #### NOTE: although we don't use them explicitely, the SUBFACE, BOLD and
;; REVEALED  initargs are declared valid below.
(defmethod initialize-instance :around
    ((face face) &rest keys &key subface bold revealed)
  "Canonicalize initialization arguments.
This involves:
- computing :subfaces initarg from the :subface ones,
- handling convenience highlight properties."
  (declare (ignore subface bold revealed))
  (apply #'call-next-method face
	 :subfaces (remove :subface (select-keys keys :subface))
	 (replace-keys keys :subface
		       '(:bold :intensity (t :bold) (nil :normal))
		       '(:revealed :concealed (t nil) (nil t)))))

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
		  &key display left-padding separator item-separator subface
		       intensity bold italicp underline blink inverse
		       concealed revealed crossed-out-p framedp foreground
		       background)
  "Make a new face named NAME."
  (declare (ignore display left-padding separator item-separator subface
		   intensity bold italicp underline blink inverse
		   concealed revealed crossed-out-p framedp foreground
		   background))
  (apply #'make-instance 'face :name name keys))

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
			  :bold t
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
