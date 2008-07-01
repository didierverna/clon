;;; group.lisp --- Group management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Jul  1 15:52:44 2008
;; Last Revision: Tue Jul  1 15:52:44 2008

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
;; Group creation
;; ============================================================================

(defstruct (group (:constructor make-group-instance))
  "The GROUP structure.
Groups contain strings, options or other groups.
When outputting help strings, groups are indented according to their nesting
level."
  (closed nil))

(defun make-group ()
  (make-group-instance))


;; ============================================================================
;; Group sealing
;; ============================================================================

(defmethod seal ((grp group))
  "Seal GRP.
A group must be sealed before it can be added to a context."
  (and (group-closed grp) (error "Group ~A already closed." grp))
  (setf (group-closed grp) t))


;;; group.lisp ends here
