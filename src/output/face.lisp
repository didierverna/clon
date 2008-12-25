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

(defstruct (face (:constructor %make-face))
  name
  item-separator
  faces)

(defun make-face (name &rest keys &key item-separator face)
  "Make a new face named NAME."
  (apply #'%make-face
    :name name
    :faces (remove :face (select-keys keys :face))
    (remove-keys keys :face)))

(defun make-faces ()
  (make-face "default"
    :item-separator #\newline
    :face (make-face "synopsis"
	    :item-separator #\space
	    :face (make-face "program")
	    :face (make-face "minus-pack")
	    :face (make-face "plus-pack")
	    :face (make-face "options")
	    :face (make-face "postfix"))
    :face (make-face "text")
    :face (make-face "option"
	    :item-separator #\newline
	    :face (make-face "syntax"
		    :item-separator ", "
		    :face (make-face "short-name"
			    :face (make-face "argument"))
		    :face (make-face "long-name"
			    :face (make-face "argument")))
	    :face (make-face "description"
		    :item-separator #\newline
		    :face (make-face "fallback")
		    :face (make-face "default")
		    :face (make-face "environment")))
    :face (make-face "group"
	    :item-separator #\newline)))


;;; face.lisp ends here
