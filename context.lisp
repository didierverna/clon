;;; context.lisp --- Context management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Jul  1 16:08:02 2008
;; Last Revision: Tue Jul  1 16:08:02 2008

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


;; ============================================================================
;; The Context class
;; ============================================================================

(defclass context (container)
  ((arglist :documentation "The argument list to process."
	    :type list
	    :accessor arglist
	    :initarg :arglist)
   (remainder :documentation "The non-Clon part of the argument list."
	      :type list
	      :accessor remainder
	      :initform nil)
   (postfix :documentation "A postfix to the program synopsis."
	    :type string
	    :reader postfix
	    :initarg :postfix)
   (minus-pack :documentation "The minus pack string."
	       :type string
	       :accessor minus-pack
	       :initform nil)
   (plus-pack :documentation "The plus pack string."
	      :type string
	      :accessor plus-pack
	      :initform nil))
  (:default-initargs
    :arglist sb-ext:*posix-argv* ;; #### FIXME: SBCL specific
    :postfix "")
  (:documentation "The CONTEXT class.
This class holds the necessary information to process a particular set of
command-line options."))

(defmethod initialize-instance :after ((context context) &rest initargs)
  "Replace the provided argument list with a copy."
  (declare (ignore initargs))
  (setf (arglist context) (copy-list (arglist context))))

;; #### FIXME: SBCL-specific
(defun make-context (&rest keys &key arglist postfix)
  "Make a new context.
- ARGLIST is the argument list (strings) to process.
  It defaults to the user-specific part of the command-line options.
  The list is copied (the original is left untouched).
- POSTFIX is a string to append to the program synopsis.
  It defaults to the empty string."
  (declare (ignore arglist postfix))
  (apply #'make-instance 'context keys))


;; ============================================================================
;; Context sealing
;; ============================================================================

(defmethod seal ((context context))
  "Seal CONTEXT."
  ;; Add the Clon internal options group
  (let ((grp (make-group)))
    (add-to grp (make-text :string "Clon specific options:"))
    (add-to grp (make-internal-flag "help" "Display Clon-specific help."))
    (add-to grp (make-internal-stropt "version" "Display Clon's version number.
WHICH can be `number', `short' or `long'."
		  :argument-name "WHICH"
		  :argument-type :optional
		  :default-value "long"
		  :env-var "VERSION_FORMAT"))
    (let ((subgrp (make-group)))
      (add-to subgrp (make-text :string "Clon output:"))
      (add-to subgrp (make-internal-stropt "search-path" "Set Clon's search path.
If you don't want any search path at all, use this option with no argument."
		       :argument-name "PATH"
		       :argument-type :optional
		       ;; #### FIXME: port DATADIR from the C version
		       :default-value "~/share/clon:"
		       :env-var "SEARCH_PATH"))
      (add-to subgrp (make-internal-stropt "theme" "Set Clon's output theme.
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
    (add-to context grp))
  ;; this calls the CONTAINER sealing method, hence performing name clash
  ;; check.
  (call-next-method)
  ;; Compute the minus and plus packs
  (do-options (option context)
    (let ((minus-char (minus-char option))
	  (plus-char (plus-char option)))
      (when minus-char
	(setf (minus-pack context)
	      (concatenate 'string (minus-pack context) minus-char)))
      (when plus-char
	(setf (plus-pack context)
	      (concatenate 'string (plus-pack context) plus-char)))))
  ;; Do a first scan of the command line, distinguish options, minus and plus
  ;; packs. Syntax checking is not done here, but only at option retrieval
  ;; time. More specifically:
  ;;
  ;; - We never try to be clever about possible misuses of the options (like,
  ;; a short name used with a double dash or stuff like that).
  ;;
  ;;  - After `--', we detect long names, possibly up to an `=' sign. Whether
  ;;  the option actually takes an argument is not checked.
  ;;
  ;;  - If the above fails, we consider that we have an unknow option. We
  ;;  don't try to see if it's a short name bogusly used, or stuff like that.
  ;;
  ;;  - After a `-', we detect short names, if not, sticky arguments, and
  ;;  otherwise, minus packs (in which case the last character is authorized
  ;;  to have an argument, but not sticky).
  ;;
  ;;  - If the above failed, we consider that we have an unknow option. We
  ;;  don't try to see if it's a long name bogusly used, or stuff like that.
  ;;
  ;;  - After a `+' we detect booleans short names, if not, plus packs (only
  ;;  booleans also).
  ;;
  ;;  - If the above failed, we consider that we have an unknow boolean
  ;;  option. We don't try to see if it's another type of option, or stuff
  ;;  like that.
  ;;
  ;;  #### NOTE: Maybe we could try *all* options names, in *all* cases, in
  ;;  order to detect the maximum of usage mistakes ?
  (let ((desclist (list (car (arglist context))))
	(arglist (cdr (arglist context))))
    (do ((arg (pop arglist) (pop arglist)))
	((null arg))
      (cond ((string= arg "--")
	     (setf (remainder context) arglist)
	     (setq arglist nil))
	    ))
    (setf (arglist context) desclist)))

;;; context.lisp ends here
