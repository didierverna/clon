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
;; The Face Structure
;; =========================================================================

(defstruct (face (:constructor %make-face))
  name
  (display :block)
  (indentation 0)
  (separator nil)
  (item-separator nil)
  (faces nil)
  (parent nil))

(defun make-face (name
		  &rest keys
		  &key display indentation separator item-separator face)
  "Make a new face named NAME."
  (declare (ignore display indentation separator item-separator face))
  (let ((new-face (apply #'%make-face
		    :name name
		    :faces (remove :face (select-keys keys :face))
		    (remove-keys keys :face))))
    (mapc (lambda (child) (setf (face-parent child) new-face))
	  (face-faces new-face))
    new-face))

(defun copy-face-tree (face)
  "Return a copy of FACE tree."
  (let ((new-face (copy-face face)))
    (setf (face-faces new-face) (mapcar #'copy-face-tree (face-faces new-face)))
    (mapc (lambda (subface) (setf (face-parent subface) new-face))
	  (face-faces new-face))
    new-face))

(defun subfacep (name face)
  "Return subface named NAME from FACE, or nil."
  (find name (face-faces face) :key #'face-name))

(defun find-face (name face)
  "Find face named NAME in face FACE.
Face should be either a direct subface of FACE (in which case it is simply
returned) or a subface of one of FACE's parents (in which case the whole face
tree is copied as a new subface of FACE)."
  (or (subfacep name face)
      (loop :for super := (face-parent face) :then (face-parent super)
	    :while super
	    :for found := (subfacep name super)
	    :when found
	    :do (let ((new-tree (copy-face-tree found)))
		  (setf (face-parent new-tree) face)
		  (push new-tree (face-faces face))
		  (return-from find-face new-tree))
	    :finally (error "Face ~A not found." name))))

;; #### FIXME: face properties should be inherited from the parent face
;; instead of provided with the structure default value.
(defun make-face-tree ()
  (make-face 'help
    :item-separator #\newline
    :face (make-face 'synopsis
	    :separator #\newline
	    :item-separator #\space
	    :face (make-face 'program :display :inline)
	    :face (make-face 'minus-pack  :display :inline)
	    :face (make-face 'plus-pack :display :inline)
	    :face (make-face 'options :display :inline)
	    :face (make-face 'postfix :display :inline))
    :face (make-face 'text)
    :face (make-face 'option
	    :indentation 2
	    :item-separator #\space
	    :face (make-face 'syntax
		    :display :inline
		    :item-separator ", "
		    :face (make-face 'short-name
			    :display :inline
			    :face (make-face 'argument :display :inline))
		    :face (make-face 'long-name
			    :display :inline
			    :face (make-face 'argument :display :inline)))
	    :face (make-face 'description
		    :item-separator #\newline
		    :face (make-face 'fallback :display :inline)
		    :face (make-face 'default :display :inline)
		    :face (make-face 'environment :display :inline)))
    :face (make-face 'group
	    :item-separator #\newline)))


;;; face.lisp ends here
