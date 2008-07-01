;;; clon.asd --- ASDF system definition for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jun 18 08:40:38 2008
;; Last Revision: Wed Jun 18 08:40:38 2008

;; This file is part of clon.

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

(in-package :cl-user)

(defpackage :clon-system
    (:use :cl :asdf)
  (:export :+version+))


(in-package :clon-system)

(defconstant +version+ "0.1"
  "Current version of Clon")

(defsystem :clon
  :version #.+version+
  :components ((:file "package")
	       (:file "clon"))
  :serial t)

;;; clon.asd ends here
