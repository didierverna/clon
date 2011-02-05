;;; switch-base.lisp --- Base for switch-like options

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Clon.

;; Redistribution and use in source or binary form, with or without
;; modification, are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.

;; Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; Neither the names of the authors or copyright holders, nor the names of any
;; contributor or organization may be used to endorse or promote products
;; derived from Clon without specific prior written permission.

;; CLON IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; ==========================================================================
;; The SWITCH-BASE Class
;; ==========================================================================

;; #### NOTE: previously, the argument-styles, yes-values and no-values slots
;; were implemented as shared slots. I changed this when porting to ABCL
;; because contrary to the other supported implementations, ABCL initialized
;; shared slot when the first instance is created instead of when the class is
;; finalized. As a consequence, this breaks the :before method below in which
;; the argument-styles slot would be unbound.
(defabstract switch-base (negatable)
  ((argument-styles :documentation "The possible argument styles.
The position of every argument style in the list must correspond to the
position of the associated strings in the yes-values and no-values slots."
		    :type list
		    :initarg :argument-styles
		    :accessor argument-styles)
   (yes-values :documentation "The possible 'yes' values."
	       :type list
	       :initarg :yes-values
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :type list
	      :initarg :no-values
	      :accessor no-values)
   (argument-style :documentation "The selected argument style."
		   :type keyword
		   :initform :yes/no
		   :initarg :argument-style
		   :reader argument-style))
  (:default-initargs
    :argument-type :optional
    :argument-styles '(:yes/no :on/off :true/false :yup/nope :yeah/nah)
    :yes-values '("yes" "on" "true" "yup" "yeah")
    :no-values '("no" "off" "false" "nope" "nah"))
  (:documentation "The SWITCH-BASE abstract class.
This class provides support for options including boolean values."))

(defmethod initialize-instance :before
    ((switch-base switch-base)
     &key (argument-style :yes/no argument-style-supplied-p) argument-styles)
  "Check for validity of the :ARGUMENT-STYLE initarg."
  (when argument-style-supplied-p
    (unless (member argument-style argument-styles)
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
