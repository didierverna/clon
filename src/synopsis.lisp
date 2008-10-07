;;; synopsis.lisp --- Synopsis management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Jul 13 11:14:19 2008
;; Last Revision: Sun Jul 13 11:14:19 2008

;; This file is part of Clon.

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

(in-package :clon)
(in-readtable :clon)


;; ============================================================================
;; The Synopsis Class
;; ============================================================================

(defclass synopsis (container)
  ((postfix :documentation "A postfix to the program synopsis."
	    :type string
	    :initarg :postfix
	    :reader postfix)
   (minus-pack :documentation "The minus pack string."
	       :type (or null string)
	       :reader minus-pack)
   (plus-pack :documentation "The plus pack string."
	      :type (or null string)
	      :reader plus-pack)
   (potential-pack :documentation "The potential pack string."
		   :type (or null string)
		   :reader potential-pack))
  (:default-initargs
      :postfix ""))

(defun make-synopsis (&rest keys &key postfix)
  "Make a new SYNOPSIS.
- POSTFIX is a string to append to the program synopsis.
  It defaults to the empty string."
  (declare (ignore postfix))
  (apply #'make-instance 'synopsis keys))


;; ----------------
;; Sealing protocol
;; ----------------

;; #### TODO: we need a way to declare explicitely that a postfix exists for
;; this synopsis. Currently, only -- or the POSIXLY_CORRECT env var can allow
;; that. If we provide an option to force a POSIXLY_CORRECT behavior, for
;; instance, we will have a problem in the cmdline parsing process, as it
;; would take the first non-option argument as a potential argument to the
;; previous option. This shows that we have to do more than just syntax
;; parsing at context creation time.

(defmethod seal :around ((synopsis synopsis))
  "Add Clon specific options to SYNOPSIS."
  (let ((grp (make-group)))
    (add-to grp (make-text :contents "Clon specific options:"))
    (add-to grp (make-internal-flag "help" "Display Clon-specific help."))
    (add-to grp (make-internal-stropt "version"
		    "Display Clon's version number.
FMT can be `number', `short' or `long'."
		  :argument-name "FMT"
		  :argument-type :optional
		  :default-value "long"
		  :env-var "VERSION_FORMAT"))
    (let ((subgrp (make-group)))
      (add-to subgrp (make-text :contents "Clon output:"))
      ;; #### TODO: extend Clon with a search-path option type converting a
      ;; search path as below into a path list.
      (add-to subgrp (make-internal-stropt "search-path"
			 "Set Clon's search path.
If you don't want any search path at all, use this option with no argument."
		       :argument-name "PATH"
		       :argument-type :optional
		       ;; #### TODO: maybe we could be more OS-friendly (read
		       ;; OS-specific) in the default-value below.
		       :default-value ~"~/.clon:"
				     ~"~/share/clon:"
				     ~"/usr/local/share/clon:"
				     ~"/usr/share/clon"
		       :env-var "SEARCH_PATH"))
      (add-to subgrp (make-internal-stropt "theme"
			 "Set Clon's output theme.
If you don't want any theme at all, use this option with no argument. Unless
starting with /, ./ or ../, files are looked for in the Clon search path. The
cth extension can be omitted."
		       :argument-name "PATH"
		       :argument-type :optional
		       :default-value "default"
		       :env-var "THEME"))
      (add-to subgrp (make-internal-stropt "line-width"
			 "Set Clon's output line width.
If not given, the terminal size will be used when possible. Otherwise, 80
columns will be assumed."
		       :argument-name "WIDTH"
		       :env-var "WIDTH"))
      (add-to subgrp (make-internal-switch "highlight"
			 "Force or inhibit Clon's output highlighting.
If not given, highlighting will be turned on for tty output, and off
otherwise."
		       :env-var "HIGHLIGHT"))
      (seal subgrp)
      (add-to grp subgrp))
    (seal grp)
    (add-to synopsis grp))
  (call-next-method))

(defmethod seal :after ((synopsis synopsis))
  "Compute the SYNOSPSIS' minus and plus packs."
  (let (potential-pack minus-pack plus-pack)
    (do-options (option synopsis)
      (let ((potential-pack-char (potential-pack-char option :as-string))
	    (minus-pack-char (minus-pack-char option :as-string))
	    (plus-pack-char (plus-pack-char option  :as-string)))
	(when potential-pack-char
	  (setq potential-pack
		(concatenate 'string potential-pack potential-pack-char)))
	(when minus-pack-char
	  (setq minus-pack (concatenate 'string minus-pack minus-pack-char)))
	(when plus-pack-char
	  (setq plus-pack (concatenate 'string plus-pack plus-pack-char)))))
    (setf (slot-value synopsis 'potential-pack) potential-pack)
    (setf (slot-value synopsis 'minus-pack) minus-pack)
    (setf (slot-value synopsis 'plus-pack) plus-pack)))


;; ============================================================================
;; The Potential Pack Protocol
;; ============================================================================

(defgeneric potential-pack-p (pack there)
  (:documentation "Return t if PACK is a potential pack in THERE.")
  (:method (pack (synopsis synopsis))
    "Return t if PACK is a potential pack for SYNOPSIS."
    ;; #### NOTE: if there's no potential pack in SYNOPSIS, the call to
    ;; STRING-LEFT-TRIM gets a nil CHAR-BAG which is ok and gives the expected
    ;; result.
    (zerop (length (string-left-trim (potential-pack synopsis) pack)))))


;; ============================================================================
;; Convenience group definition
;; ============================================================================

(defmacro define-synopsis (synopsis (&rest keys) &body body)
  "Evaluate BODY with SYNOPSIS bound to a new synopsis, seal it and return it.
KEYS are passed to `make-synopsis'."
  (push 'make-synopsis keys)
  `(let ((,synopsis ,keys))
    ,@body
    (seal ,synopsis)
    ,synopsis))

(defmacro declare-synopsis ((&rest keys) &body forms)
  "Define a new synopsis, add FORMS to it, seal it and return it.
FORMS should be a list of shortcut expressions matching calls to make-group,
make-text, or make-<option>, only with the 'make-' prefix omitted. Each
resulting group, text or option created will be automatically added to the
synopsis."
  (let* ((synopsis (gensym "synopsis"))
	 (forms (mapcar (lambda (form)
			 (list (intern "ADD-TO" 'clon) synopsis form))
		       forms))
	 (group (intern "GROUP"))
	 (text (intern "TEXT"))
	 (flag (intern "FLAG")))
    `(macrolet ((,group (&rest args) `(declare-group ,@args))
		(,text (&rest args) `(make-text ,@args))
		(,flag (&rest args) `(make-flag ,@args))
		,@(mapcar
		   (lambda (name)
		     (let ((macro-name (intern name))
			   (make-name (intern (concatenate 'string "MAKE-" name)
					      'clon)))
		       `(,macro-name (&rest args) `(,',make-name ,@args))))
		   *valued-option-names*))
      (define-synopsis ,synopsis ,keys
	,@forms))))


;;; synopsis.lisp ends here
