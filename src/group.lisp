;;; group.lisp --- Group management

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
;; The Group class
;; ==========================================================================

(defclass group (container)
  ((header :documentation "The group's header."
	   :initform nil
	   :initarg :header
	   :reader header))
  (:documentation "The GROUP class.
This class groups other groups, options or strings together, effectively
implementing hierarchical program command-line."))


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((group group) &key)
  "Return GROUP's help specification."
  (accumulate (group)
    (accumulate (header)
      (header group))
    ;; This calls the container's method.
    (let ((items (call-next-method)))
      (when items
	(push 'items items)))))



;; ==========================================================================
;; Group Instance Creation
;; ==========================================================================

(defun make-group (&rest keys &key header item hidden)
  "Make a new group."
  (declare (ignore header item hidden))
  (apply #'make-instance 'group keys))

(defmacro %defgroup (internalp (&rest keys &key header hidden) &body forms)
  "Define a new group."
  (declare (ignore header hidden))
  `(make-group ,@keys
    ,@(loop :for form :in forms
	    :nconc (list :item
			 (let ((item-name
				(when (consp form)
				  (car (member (symbol-name (car form))
					       *item-names*
					       :test #'string=)))))
			   (if item-name
			       (list* (intern
				       (cond ((string= item-name "GROUP")
					      "%DEFGROUP")
					     (t
					      (format nil
						  "MAKE-~:[~;INTERNAL-~]~A"
						internalp item-name)))
				       :com.dvlsoft.clon)
				      (if (string= item-name "GROUP")
					  (list* internalp (cdr form))
					(cdr form)))
			     form))))))

(defmacro defgroup ((&rest keys &key header hidden) &body forms)
  "Define a new group.
KEYS are initargs to MAKE-GROUP (currently, only :header).
Each form in FORMS will be treated as a new :item.
The CAR of each form is the name of the operation to perform: TEXT, GROUP, or
an option class name. The rest are the arguments to the MAKE-<OP> function or
the DEFGROUP macro."
  (declare (ignore header hidden))
  `(%defgroup nil ,keys ,@forms))


;;; group.lisp ends here
