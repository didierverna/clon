;;; enum-base.lisp --- Base for enumeration-like options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 11 21:34:31 2010
;; Last Revision: Wed Aug 11 21:35:14 2010

;; This file is part of Clon.

;; Clon is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
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
;; The ENUM-BASE Class
;; ==========================================================================

(defabstract enum-base ()
  ((enum :documentation "The set of possible values."
	 :initarg :enum
	 :reader enum))
  (:documentation "The ENUM-BASE abstract class.
This class provides support for options including enumerated values."))

(defmethod initialize-instance :before ((enum-base enum-base) &key enum)
  (unless enum
    (error "Enum based option ~S: empty enum." enum-base)))


;;; enum-base.lisp ends here
