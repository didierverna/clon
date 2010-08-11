;;; switch-base.lisp --- Base for switch-like options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Aug 11 21:31:02 2010
;; Last Revision: Wed Aug 11 21:31:58 2010

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
;; The SWITCH-BASE Class
;; ==========================================================================

;; #### FIXME: consider defaulting nullablep to t here instead of in SWITCH
;; and XSWITCH. Same for ARGUMENT-TYPE -> OPTIONAL.

(defabstract switch-base (plus-callable)
  ((yes-values :documentation "The possible 'yes' values."
	       :allocation :class
	       :type list
	       :initform '("yes" "on" "true" "yup" "yeah")
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :allocation :class
	      :type list
	      :initform '("no" "off" "false" "nope" "nah")
	      :accessor no-values))
  (:documentation "The SWITCH-BASE abstract class.
This class provides support for options including boolean values."))

(defmethod initialize-instance :around
    ((switch-base switch-base) &rest keys &key argument-type)
  "Provide a fallback value of t when argument is optional."
  (when (eq argument-type :optional)
    (setq keys (list* :fallback-value t keys)))
  (apply #'call-next-method switch-base keys))


;;; switch-base.lisp ends here
