;;; readtable.lisp --- Clon readtable management

;; Copyright (C) 2021 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :net.didierverna.clon.setup)



;; ==========================================================================
;; Code indentation
;; ==========================================================================

(defun clindent (symbol indent)
  "Send SYMBOL's INDENTation information to Emacs.
Emacs will set the 'common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition. Otherwise, INDENT is
considered as an indentation definition."
  (when (and (member :swank *features*) (configuration :swank-eval-in-emacs))
    ;; #### NOTE: case portability
    (funcall (intern (string :eval-in-emacs) :swank)
      `(put ',symbol 'common-lisp-indent-function
	    ,(if (symbolp indent)
	       `(get ',indent 'common-lisp-indent-function)
	       `',indent))
      t)))

(defmacro defindent (symbol indent)
  "Wrapper around `clindent' to avoid quoting SYMBOL and INDENT."
  `(eval-when (:compile-toplevel :execute :load-toplevel)
     (clindent ',symbol ',indent)))

(defun i-reader (stream subchar arg)
  "Construct a call to `defindent' by reading an argument list from STREAM.
This dispatch macro character function is installed on #i in the
NET.DIDIERVERNA.CLON named readtable."
  (declare (ignore subchar arg))
  (cons 'defindent (read stream)))



;; ==========================================================================
;; String Concatenation
;; ==========================================================================

(defun ~-reader (stream char)
  "Read a series of ~\"string\" to be concatenated together."
  (declare (ignore char))
  (flet ((read-string (&aux (string (read stream t nil t)))
	   (unless *read-suppress* (check-type string string "a string"))
	   string))
    (apply #'concatenate 'string
	   (read-string)
	   (loop :while (char= (peek-char t stream nil nil t) #\~)
		 :do (read-char stream t nil t)
		 :collect (read-string)))))



;; ==========================================================================
;; Named Readtable
;; ==========================================================================

;; ECL, CLISP, Allegro and LispWorks do not like to see undefined reader
;; macros in expressions that belong to other compilers. For instance this
;; will break: #+ccl (#_ccl-only-function) It seems to be a correct behavior
;; (see *read-suppress* in CLHS), although other implementations like SBCL and
;; CMUCL are more gentle. The solution I use is to define those reader macros
;; to simply return nil.

;; #### PORTME.
#+(or ecl clisp allegro lispworks)
(progn
  (defun dummy-reader (stream subchar args)
    "Return nil."
    (declare (ignore stream subchar args))
    nil)
  (named-readtables:defreadtable dummy
    (:macro-char #\# :dispatch t)
    (:dispatch-macro-char #\# #\_ #'dummy-reader)
    ;; ECL has this one for random generator initialization.
    #-ecl (:dispatch-macro-char #\# #\$ #'dummy-reader)))

(defreadtable :net.didierverna.clon
  (:merge :current
	  ;; #### PORTME.
	  #+(or ecl clisp allegro lispworks)
	  dummy)
  (:macro-char #\~ #'~-reader)
  (:dispatch-macro-char #\# #\i #'i-reader))

;;; readtable.lisp ends here
