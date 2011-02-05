;;; text.lisp --- Text management

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
;; The Text Class
;; ==========================================================================

(defclass text (item)
  ((contents :documentation "The actual text string."
	     :type string
	     :initarg :contents
	     :reader contents))
  (:documentation "The TEXT class.
This class implements plain text objects appearing in a synopsis."))


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((text text))
  "TEXT is a terminal object: just return it."
  text)


;; ---------------------------
;; Help specification protocol
;; ---------------------------

(defmethod help-spec ((text text) &key)
  "Return TEXT's help specification."
  (accumulate (text)
    (contents text)))



;; ==========================================================================
;; Text Instance Creation
;; ==========================================================================

(defun make-text (&rest keys &key contents hidden)
  "Make a new text.
- CONTENTS is the actual text to display.
- When HIDDEN, the text doesn't appear in help strings."
  (declare (ignore contents hidden))
  (apply #'make-instance 'text keys))

;; #### NOTE: currently, there is no difference between internal and external
;; text, but this might change in the future. Besides, having this function it
;; simplifies the defgroup and defsynopsis macros.
(defun make-internal-text (&rest keys &key contents hidden)
  (declare (ignore contents hidden))
  (apply #'make-text keys))

;;; text.lisp ends here
