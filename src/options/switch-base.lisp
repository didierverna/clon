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

(defabstract switch-base (negatable)
  ((argument-styles :documentation "The possible argument styles.
The position of every argument style in the list must correspond to the
position of the associated strings in the yes-values and no-values slots."
		    :allocation :class
		    :type list
		    :initform '(:yes/no :on/off :true/false :yup/nope
				:yeah/nah)
		    :accessor argument-styles)
   (yes-values :documentation "The possible 'yes' values."
	       :allocation :class
	       :type list
	       :initform '("yes" "on" "true" "yup" "yeah")
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :allocation :class
	      :type list
	      :initform '("no" "off" "false" "nope" "nah")
	      :accessor no-values)
   (argument-style :documentation "The selected argument style."
		   :type keyword
		   :initform :yes/no
		   :initarg :argument-style
		   :reader argument-style))
  (:default-initargs
      :argument-type :optional)
  (:documentation "The SWITCH-BASE abstract class.
This class provides support for options including boolean values."))

(defmethod initialize-instance :before
    ((switch-base switch-base)
     &key (argument-style :yes/no argument-style-supplied-p))
  "Check for validity of the :ARGUMENT-STYLE initarg."
  (when argument-style-supplied-p
    (unless (member argument-style (argument-styles switch-base))
      (error "Invalid argument style initarg: ~S." argument-style))))

;; #### NOTE: ideally, I would like to do this in an :after method. But I
;; can't because of the :before method for the VALUED-OPTION class which
;; checks that when the argument type is optional, there's at least a fallback
;; or a default value provided. Sometimes, I think I'm too maniac with checks.
;; If I were to do all this is in :after methods, I would be stuck as well
;; because the one for the VALUED-OPTION class would be called *before* the
;; one for the SWITCH-BASED class. :-(
(defmethod initialize-instance :around
    ((switch-base switch-base) &rest keys &key argument-type)
  "Provide a fallback value of t when ARGUMENT-TYPE is optional."
  (when (eq argument-type :optional)
    (setq keys (list* :fallback-value t keys)))
  (apply #'call-next-method switch-base keys))


;;; switch-base.lisp ends here
