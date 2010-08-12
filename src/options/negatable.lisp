;;; negatable.lisp --- Negatable options

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
;; The Negatable Class
;; ==========================================================================

;; #### NOTE: currently, only the yes/no hierarchy is negatable (the negated
;; syntax meaning to return nil). The need for other kinds of negatable
;; options might pop up someday. Then, it would be advisable to design a
;; mechanism by which a + call would return something else than nil, actually
;; defined by the option class. A slot in the class below would do I think.
(defclass negatable ()
  ()
  (:documentation "The NEGATABLE Class.
This class implements the negated syntax for the switch-based hierarchy."))


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod short-syntax-help-spec-prefix ((option negatable))
  "-(+)")


;; -------------------
;; Char packs protocol
;; -------------------

(defmethod negated-pack-char ((negatable negatable) &optional as-string)
  "Return NEGATABLE's negated pack character, if any."
  (potential-pack-char negatable as-string))


;;; negatable.lisp ends here
