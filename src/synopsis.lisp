;;; synopsis.lisp --- Synopsis management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Jul 13 11:14:19 2008
;; Last Revision: Wed Nov  5 10:41:26 2008

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


;; ==========================================================================
;; The Option Mapping Protocol
;; ==========================================================================

;; #### NOTE: there are two very good reasons for updating the traversal state
;; of traversable objects in an after method, as done below:
;; 1/ this is obviously the right thing to do,
;; 2/ moving it away from the primary method makes this primary method return
;;    the value of FUNC itself when FUNC is actually called. This is an
;;    important idiom because callers might want to rely on the return value
;;    of (the last computation of) mapoptions, especially when used through
;;    the DO-OPTIONS below. See for example the function SEARCH-OPTION-BY-NAME
;;    in context.lisp.
(defgeneric mapoptions (func there)
  (:documentation "Map FUNC over all options in THERE.")
  (:method (func elsewhere)
    "Do nothing by default."
    (values))
  (:method :after (func (traversable traversable))
    "Mark TRAVERSABLE as traversed."
    (setf (traversedp traversable) t))
  (:method (func (container container))
    "Map FUNC over all containers or options in CONTAINER."
    (unless (traversedp container)
      (dolist (item (container-items container))
	(mapoptions func item))))
  (:method (func (option option))
    "Call FUNC on OPTION."
    (unless (traversedp option)
      (funcall func option))))

(defmacro do-options ((opt there) &body body)
  "Execute BODY with OPT bound to every option in THERE."
  `(mapoptions (lambda (,opt) ,@body)
    (untraverse ,there)))



;; ==========================================================================
;; The Synopsis Class
;; ==========================================================================

(defclass synopsis (container)
  ((postfix :documentation "A postfix to the program synopsis."
	    :type (or null string)
	    :initarg :postfix
	    :initform nil
	    :reader postfix)
   (minus-pack :documentation "The minus pack string."
	       :type (or null string)
	       :reader minus-pack)
   (plus-pack :documentation "The plus pack string."
	      :type (or null string)
	      :reader plus-pack)
   (potential-pack :documentation "The potential pack string."
		   :type (or null string)
		   :reader potential-pack)))


;; ----------------
;; Sealing protocol
;; ----------------

(defmethod seal :around ((synopsis synopsis))
  "Add Clon specific options to SYNOPSIS."
  (let ((grp (make-group)))
    (add-to grp (make-text :contents "Clon specific options:"))
    (add-to grp (make-internal-flag "banner" "Display the full Clon banner."))
    (add-to grp (make-internal-enum "version"
		    "Display Clon's version number.
FMT can be `number', `short' or `long'."
		  :argument-name "FMT"
		  :argument-type :optional
		  :enum '(:number :short :long)
		  :fallback-value :long
		  #|:env-var "VERSION_FORMAT"|#))
    (add-to grp (make-internal-flag "help" "Display Clon-specific help."))
    (let ((subgrp (make-group)))
      (add-to subgrp (make-text :contents "Clon output:"))
      (add-to subgrp (make-internal-path "search-path"
			 "Set Clon's search path.
If you don't want any search path at all, use this option with no argument."
		       :argument-type :optional
		       :fallback-value nil ;; paths are nullable by default
		       ;; #### PORTME. I'm using Unix-like default for
		       ;; everything here, plus OSX specific values that I
		       ;; know of. Not sure about Windows or anything else.
		       :default-value
		       (let ((local-path '(".clon/" "share/clon/"))
			     (global-path '(#p"/usr/local/share/clon/"
					    #p"/usr/share/clon/")))
			 (when (string= (software-type) "Darwin")
			   (push "Library/Application Support/Clon/"
				 local-path)
			   (push #p"/Library/Application Support/Clon/"
				 global-path))
			 (append
			  (mapcar
			   (lambda (subdir)
			     (merge-pathnames subdir (user-homedir-pathname)))
			   local-path)
			  global-path))
		       :env-var "SEARCH_PATH"))
      (add-to subgrp (make-internal-stropt "theme"
			 "Set Clon's output theme.
If you don't want any theme at all, use this option with no argument. Unless
starting with /, ./ or ../, files are looked for in the Clon search path. The
cth extension can be omitted."
		       :argument-name "FILE"
		       :argument-type :optional
		       :nullablep t
		       :fallback-value nil
		       :default-value "default"
		       :env-var "THEME"))
      (add-to subgrp (make-internal-stropt "line-width"
			 "Set Clon's output line width.
If not given, the terminal size will be used when possible. Otherwise, 80
columns will be assumed."
		       :argument-name "WIDTH"
		       :env-var "WIDTH"))
      (add-to subgrp (make-internal-xswitch "highlight"
			 "Set Clon's output highlighting to on/off/auto.
Auto (the default) means on for tty output and off otherwise."
		       :enum '(:auto)
		       :env-var "HIGHLIGHT"
		       :default-value :auto))
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



;; ==========================================================================
;; The Potential Pack Protocol
;; ==========================================================================

;; #### NOTE: a generic function is a bit overkill here, because its use is
;; only to provide a convenience wrapper for contexts.
(defgeneric potential-pack-p (pack there)
  (:documentation "Return t if PACK is a potential pack in THERE.")
  (:method (pack (synopsis synopsis))
    "Return t if PACK is a potential pack for SYNOPSIS."
    ;; #### NOTE: if there's no potential pack in SYNOPSIS, the call to
    ;; STRING-LEFT-TRIM gets a nil CHAR-BAG which is ok and gives the expected
    ;; result.
    (zerop (length (string-left-trim (potential-pack synopsis) pack)))))



;; ==========================================================================
;; Synopsis Instance Creation
;; ==========================================================================

(defun make-synopsis (&rest keys &key postfix)
  "Make a new SYNOPSIS.
- POSTFIX is a string to append to the program synopsis, in case it accepts a
remainder."
  (declare (ignore postfix))
  (apply #'make-instance 'synopsis keys))

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
