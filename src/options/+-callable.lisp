;;; +-callable.lisp --- Plus callable options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 11 21:26:04 2010
;; Last Revision: Wed Aug 11 21:36:52 2010

;; This file is part of Clon.

;; Clon is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License, version 3,
;; as published by the Free Software Foundation.

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

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; ==========================================================================
;; The Plus Callable Class
;; ==========================================================================

;; #### NOTE: currently, only the yes/no hierarchy is plus-callable (the +
;; syntax meaning to return nil). The need for other kinds of plus-callable
;; options might pop up someday. Then, it would be advisable to design a
;; mechanism by which a + call would return something else than nil, actually
;; defined by the option class. A slot in the class below would do I think.
(defclass plus-callable ()
  ()
  (:documentation "The PLUS-CALLABLE Class.
This class is a mixin used to authorize the +-syntax for the switch hierarchy."))


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod short-syntax-help-spec-prefix ((option plus-callable))
  "-(+)")


;; -------------------
;; Char packs protocol
;; -------------------

(defmethod plus-pack-char ((plus-callable plus-callable) &optional as-string)
  "Return PLUS-CALLABLE plus pack character, if any."
  (potential-pack-char plus-callable as-string))


;;; +-callable.lisp ends here
