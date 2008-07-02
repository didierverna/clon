;;; context.lisp --- Context management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Jul  1 16:08:02 2008
;; Last Revision: Tue Jul  1 16:08:02 2008

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
;; The Context class
;; ============================================================================

(defclass context (container)
  ((arglist :documentation "The argument list to process."
	    ;; There is at least the program pathname in the list.
	    :type list
	    :accessor context-arglist
	    :initarg :arglist)
   (postfix :documentation "A postfix to the program synopsis."
	    :type string
	    :reader context-postfix
	    :initarg :postfix))
  (:documentation "The CONTEXT class.
This class holds the necessary information to process a particular set of
command-line options."))

;; #### FIXME: SBCL-specific
(defun make-context (&key (arglist sb-ext:*posix-argv*) (postfix ""))
  "Make a new context.
- ARGLIST is the argument list (strings) to process.
  It defaults to the user-specific part of the command-line options.
  The list is copied (the original is left untouched).
- POSTFIX is a string to append to the program synopsis.
  It defaults to the empty string."
  (make-instance 'context
    :arglist (copy-list arglist)
    :postfix postfix))


;; ============================================================================
;; Context sealing
;; ============================================================================

(defmethod seal ((context context))
  "Seal context CONTEXT."
  ;; #### FIXME: do some stuff
  (values))


;;; context.lisp ends here
