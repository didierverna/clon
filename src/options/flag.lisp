;;; flag.lisp --- Flag options

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
;; The Flag Class
;; ==========================================================================

(defclass flag (option)
  ()
  (:documentation "The FLAG class.
This class implements options that don't take any argument."))



;; ==========================================================================
;; Flag Instance Creation
;; ==========================================================================

(defun make-flag (&rest keys &key short-name long-name description env-var
				  hidden)
  "Make a new flag.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the flag's associated environment variable.
  It defaults to nil.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description env-var hidden))
  (apply #'make-instance 'flag keys))

(defun make-internal-flag (long-name description
			   &rest keys &key env-var hidden)
  "Make a new internal (Clon-specific) flag.
- LONG-NAME is the flag's long-name, sans the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the flag's description.
- ENV-VAR is the flag's associated environment variable, sans the 'CLON_'
  prefix. It default to nil.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore env-var hidden))
  (apply #'make-instance 'flag
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; flag.lisp ends here
