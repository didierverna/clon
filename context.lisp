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
;; Context creation
;; ============================================================================

(defclass context (container)
  "The CONTEXT class.
This class holds the necessary information to process a particular set of
command-line options."
  (arglist nil :type list)
  (postfix nil :type string))

;; #### FIXME: SBCL-specific
(defun make-context (&key (arglist sb-ext:*posix-argv*) (postfix ""))
  "Make a new context.
- ARGLIST is the list of options (strings) to process.
  It defaults to the user-specific command-line options.
  The list is copied (the original is left untouched).
- POSTFIX is a string to append to the one-line help.
  It defaults to the empty string."
  (make-instance 'context
    :arglist (copy-list arglist)
    :postfix postfix))


;;; context.lisp ends here
