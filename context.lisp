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
;; The Command-Line Structures
;; ============================================================================

(defstruct cmdline-option
  (name) ;; the name as it appears on the command line
  (option) ;; the actual option is corresponds to, or null if unknown
  (value) ;; the option's value as it appears on the command line
  )

(defstruct cmdline-pack
  (type) ;; either :minus or :plus
  (contents) ;; the contents (characters) of the pack
  )


;; ============================================================================
;; The Context Class
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


;; ----------------
;; Sealing protocol
;; ----------------

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
  ;; Perform syntactic analysis of the command-line: spot options, option
  ;; values, minus and plus packs, and isolate the non-Clon part. Semantic
  ;; analysis (extra/missing option values, value conversion error etc) is
  ;; done when options are actually retrieved.
  ;;
  ;; More specifically:
  ;;
  ;; - We never try to be clever about possible misuses of the options (like,
  ;; a short name used with a double dash or stuff like that).
  ;;
  ;;  - After a `+' detect in turn booleans short names or plus packs (only
  ;;  booleans in there).
  ;;
  ;;  - If the above fails, consider that we have an unknow boolean option.
  ;;
  ;;  #### NOTE: currently, name clashes are considered on short and long
  ;;  names independently. That is, it is possible to have a short name
  ;;  identical to a long one, although I don't see why you would want to do
  ;;  that.
  (let ((desclist (list (car (arglist context))))
	(arglist (cdr (arglist context))))
    (do ((arg (pop arglist) (pop arglist)))
	((null arg))
      (cond ((string= arg "--")
	     ;; The Clon separator:
	     ;; Isolate the rest of the command line.
	     (setf (remainder context) arglist)
	     (setq arglist nil))
	    ;; A long (possibly unknown) option:
	    ((string-start arg "--")
	     (let* ((value-start (position #\= arg :start 2))
		    (name (subseq arg 2 value-start))
		    ;; #### NOTE: we authorize partial matching (abreviations)
		    ;; for long names: an exact match is search first. If that
		    ;; fails, we try partial matching, and the first matching
		    ;; option is returned. For instance, if you have --foobar
		    ;; and --foo options in that order, passing --foo will
		    ;; match the option --foo, but passing --fo will match
		    ;; --foobar. I'm not sure this is the best behavior in
		    ;; such cases. Think harder about this.
		    (option (or (search-option context :long-name name)
				(search-option context :partial-name name)))
		    (value (if value-start
			       (subseq arg (1+ value-start))
			       (unless (or (eq (elt (car arglist) 0) #\-)
					   (eq (elt (car arglist) 0) #\+))
				 (pop arglist)))))
	       ;; #### NOTE: OPTION might be nil if it is unknown to Clon.
	       ;; What to do with unknown options is up to the user. We don't
	       ;; do anyting special here.
	       (endpush (make-cmdline-option
			 :name (or (when option (long-name option)) name)
			 :option option :value value)
			desclist)))
	    ;; A short (possibly unknown) option or a minus pack:
	    ((string-start arg "-")
	     (let ((name (subseq arg 1))
		   option)
	       (cond ((setq option (search-option context :short-name name))
		      ;; We found an option:
		      (endpush
		       (make-cmdline-option
			:name name
			:option option
			:value (unless (or (eq (elt (car arglist) 0) #\-)
					   (eq (elt (car arglist) 0) #\+))
				 (pop arglist)))
		       desclist))
		     ((setq option (search-sticky-option context name))
		      ;; We found an option with a sticky argument.
		      ;; #### NOTE: when looking for a sticky option, we stop
		      ;; at the first match, even if, for instance, another
		      ;; option would match a longer part of the argument. I'm
		      ;; not sure this is the best behavior in such cases.
		      ;; Think harder about this.
		      (endpush (make-cmdline-option
				:name (short-name option)
				:option option
				:value (subseq name
					       (length (short-name option))))
			       desclist))
		     ((minus-pack context)
		      ;; Let's find out whether it is a minus pack:
		      (let ((trimmed (string-left-trim (minus-pack context)
						       name)))
			(cond ((zerop (length trimmed))
			       ;; We found a simple minus pack
			       (endpush
				(make-cmdline-pack :type :minus :contents name)
				desclist))
			      ((= (length trimmed) 1)
			       ;; There's one character left: maybe a last
			       ;; option in the pack requiring an argument
			       ;; (remember that those options don't appear in
			       ;; the minus pack description).
			       (setq option (search-option context
					      :short-name trimmed))
			       (cond (option
				      ;; We found an option. Separate the pack
				      ;; into a simple minus pack, and a
				      ;; cmdline option:
				      (endpush
				       (make-cmdline-pack
					:type :minus
					:contents (subseq name 0
							  (1- (length name))))
				       desclist)
				      (endpush
				       (make-cmdline-option
					:name trimmed
					:option option
					:value
					(unless (or (eq (elt (car arglist) 0)
							#\-)
						    (eq (elt (car arglist) 0)
							#\+))
					  (pop arglist)))
				       desclist))
				     ;; The last character doesn't correspond
				     ;; to a known option. Consider the whole
				     ;; pack as an unknown option.
				     (t
				      (endpush (make-cmdline-option
						:name name
						:value
						(unless (or (eq (elt
								 (car arglist)
								 0)
								#\-)
							    (eq (elt
								 (car arglist)
								 0)
								#\+))
						  (pop arglist)))
					       desclist))))
			      (t
			       ;; There's more than one character left. As
			       ;; above, consider the whole pack as an unknown
			       ;; option.
			       (endpush (make-cmdline-option
					 :name name
					 :value
					 (unless (or (eq (elt (car arglist) 0)
							 #\-)
						     (eq (elt (car arglist) 0)
							 #\+))
					   (pop arglist)))
					desclist))))))))
	    ;; A short (possibly unknown) switch, or a plus pack.
	    ((string-start arg "+")
	     (let ((name (subseq arg 1))
		   option)
	       (cond ((setq option (search-option context :short-name name))
		      ;; We found an option.
		      ;; #### NOTE: the value we assign to the option here is
		      ;; "no" since the option is supposed to be a switch. In
		      ;; case of a usage error (not a switch), we have a
		      ;; problem that might or might not be detected at
		      ;; retrieval time: flags would notice an extra value, a
		      ;; user converter might notice an invalid value, but a
		      ;; simple stropt would not notice anything.
		      ;; We could try to be more clever and detect that the
		      ;; option's type is wrong, but I'm too lazy to do that
		      ;; right now.
		      (endpush (make-cmdline-option
				:name name :option option :value "no")
			       desclist))
		     ((plus-pack context)
		      ;; Let's find out whether it is a plus pack:
		      (let ((trimmed (string-left-trim (plus-pack context) name)))
			(cond ((zerop (length trimmed))
			       ;; We found a plus pack
			       (endpush
				(make-cmdline-pack :type :plus :contents name)
				desclist))
			      (t
			       ;; We found an unknown switch:
			       (endpush
				(make-cmdline-option :name name :value "no")
				desclist))))))))
	    ;; Otherwise, it's junk.
	    (t
	     ;; #### FIXME: SBCL specific.
	     (cond ((sb-ext:posix-getenv "POSIXLY_CORRECT")
		    ;; That's the end of the Clon-specific part:
		    (setf (remainder context) (cons arg arglist))
		    (setq arglist nil))
		   (t
		    ;; Otherwise, that's really junk:
		    (endpush arg desclist))))))
    (setf (arglist context) desclist)))

;;; context.lisp ends here
