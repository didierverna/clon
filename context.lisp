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

;; #### FIXME: make final
;; #### FIXME: the ugliness of using arglist to successively store different
;; things as I do is a demonstration that this stuff is not well abstracted.
;; We should have a separate description of the options somewhere, and then
;; contexts using this description to work on a specific cmdline. What's shaky
;; right now is that the cmdline is provided at context creation, but really
;; used only when sealing.
(defclass context ()
  ((synopsis :documentation "The program synopsis."
	     :type synopsis
	     :reader synopsis
	     :initarg :synopsis)
   (progname :documentation "The program name, as it appears on the command line."
	     :type string
	     :reader progname)
   (arglist :documentation "The argument list to process."
	    :type list
	    :accessor arglist)
   (remainder :documentation "The non-Clon part of the argument list."
	      :type list
	      :accessor remainder
	      :initform nil))
  (:default-initargs
      ;; #### FIXME: SBCL specific
      :cmdline sb-ext:*posix-argv*)
  (:documentation "The CONTEXT class.
This class holds the necessary information to process a particular set of
command-line options."))

(defmethod initialize-instance :before ((context context) &key synopsis cmdline)
  "Ensure that SYNOPSIS is sealed."
  (declare (ignore cmdline))
  (unless (sealedp synopsis)
    (error "Initializing context ~A: synopsis ~A not sealed." context synopsis)))

(defmethod initialize-instance :after ((context context) &key synopsis cmdline)
  "Parse CMDLINE.
Extract the program name, construct the parsed argument list and the remainder."
  (declare (ignore synopsis))
  (setf (slot-value context 'progname) (pop cmdline))
  ;; Perform syntactic analysis of the command-line: spot options, option
  ;; values, minus and plus packs, and isolate the non-Clon part. Semantic
  ;; analysis (extra/missing option values, value conversion error etc) is
  ;; done when options are actually retrieved.
  ;;
  ;;  #### NOTE: currently, name clashes are considered on short and long
  ;;  names independently. That is, it is possible to have a short name
  ;;  identical to a long one, although I don't see why you would want to do
  ;;  that.
  (let ((arglist (list)))
    (macrolet ((maybe-next-cmdline-arg ()
		 '(unless (or (eq (elt (car cmdline) 0) #\-)
			   (eq (elt (car cmdline) 0) #\+))
		   (pop cmdline))))
      (do ((arg (pop cmdline) (pop cmdline)))
	  ((null arg))
	(cond ((string= arg "--")
	       ;; The Clon separator:
	       ;; Isolate the rest of the command line.
	       (setf (remainder context) cmdline)
	       (setq cmdline nil))
	      ;; A long (possibly unknown) option:
	      ((string-start arg "--")
	       (let* ((value-start (position #\= arg :start 2))
		      (name (subseq arg 2 value-start))
		      ;; #### NOTE: we authorize partial matching
		      ;; (abreviations) for long names: an exact match is
		      ;; search first. If that fails, we try partial matching,
		      ;; and the first matching option is returned. For
		      ;; instance, if you have --foobar and --foo options in
		      ;; that order, passing --foo will match the option
		      ;; --foo, but passing --fo will match --foobar. I'm not
		      ;; sure this is the best behavior in such cases. Think
		      ;; harder about this.
		      (option (or (search-option (synopsis context)
				    :long-name name)
				  (search-option (synopsis context)
				    :partial-name name)))
		      (value (if value-start
				 (subseq arg (1+ value-start))
				 (maybe-next-cmdline-arg))))
		 ;; #### NOTE: OPTION might be nil if it is unknown to Clon.
		 ;; What to do with unknown options is up to the user. We
		 ;; don't do anyting special here.
		 (push (make-cmdline-option
			:name (or (when option (long-name option)) name)
			:option option :value value)
		       arglist)))
	      ;; A short (possibly unknown) option or a minus pack:
	      ((string-start arg "-")
	       (let ((name (subseq arg 1))
		     option)
		 (cond ((setq option (search-option (synopsis context)
				       :short-name name))
			;; We found an option:
			(push (make-cmdline-option
			       :name name
			       :option option
			       :value (maybe-next-cmdline-arg))
			      arglist))
		       ((setq option (search-sticky-option (synopsis context)
							   name))
			;; We found an option with a sticky argument.
			;; #### NOTE: when looking for a sticky option, we
			;; stop at the first match, even if, for instance,
			;; another option would match a longer part of the
			;; argument. I'm not sure this is the best behavior in
			;; such cases. Think harder about this.
			(push (make-cmdline-option
			       :name (short-name option)
			       :option option
			       :value (subseq name
					      (length (short-name option))))
			      arglist))
		       ((minus-pack (synopsis context))
			;; Let's find out whether it is a minus pack:
			(let ((trimmed (string-left-trim
					(minus-pack (synopsis context))
					name)))
			  (cond ((zerop (length trimmed))
				 ;; We found a simple minus pack
				 (push (make-cmdline-pack
					:type :minus :contents name)
				       arglist))
				((= (length trimmed) 1)
				 ;; There's one character left: maybe a last
				 ;; option in the pack requiring an argument
				 ;; (remember that those options don't appear
				 ;; in the minus pack description).
				 (setq option (search-option (synopsis context)
						:short-name trimmed))
				 (cond (option
					;; We found an option. Separate the
					;; pack into a simple minus pack, and
					;; a cmdline option:
					(push
					 (make-cmdline-pack
					  :type :minus
					  :contents (subseq name 0
							    (1- (length name))))
					 arglist)
					(push
					 (make-cmdline-option
					  :name trimmed
					  :option option
					  :value
					  (maybe-next-cmdline-arg))
					 arglist))
				       ;; The last character doesn't
				       ;; correspond to a known option.
				       ;; Consider the whole pack as an
				       ;; unknown option.
				       (t
					(push (make-cmdline-option
					       :name name
					       :value
					       (maybe-next-cmdline-arg))
					      arglist))))
				(t
				 ;; There's more than one character left. As
				 ;; above, consider the whole pack as an
				 ;; unknown option.
				 (push (make-cmdline-option
					:name name
					:value
					(maybe-next-cmdline-arg))
				       arglist))))))))
	      ;; A short (possibly unknown) switch, or a plus pack.
	      ((string-start arg "+")
	       (let ((name (subseq arg 1))
		     option)
		 (cond ((setq option (search-option (synopsis context)
				       :short-name name))
			;; We found an option.
			;; #### NOTE: the value we assign to the option here
			;; is "no" since the option is supposed to be a
			;; switch. In case of a usage error (not a switch), we
			;; have a problem that might or might not be detected
			;; at retrieval time: flags would notice an extra
			;; value, a user converter might notice an invalid
			;; value, but a simple stropt would not notice
			;; anything. We could try to be more clever and detect
			;; that the option's type is wrong, but I'm too lazy
			;; to do that right now.
			(push (make-cmdline-option
			       :name name :option option :value "no")
			      arglist))
		       ((plus-pack (synopsis context))
			;; Let's find out whether it is a plus pack:
			(let ((trimmed (string-left-trim
					(plus-pack (synopsis context))
					name)))
			  (cond ((zerop (length trimmed))
				 ;; We found a plus pack
				 (push
				  (make-cmdline-pack :type :plus :contents name)
				  arglist))
				(t
				 ;; We found an unknown switch:
				 (push
				  (make-cmdline-option :name name :value "no")
				  arglist))))))))
	      ;; Otherwise, it's junk.
	      (t
	       ;; #### FIXME: SBCL specific.
	       (cond ((sb-ext:posix-getenv "POSIXLY_CORRECT")
		      ;; That's the end of the Clon-specific part:
		      (setf (remainder context) (cons arg cmdline))
		      (setq cmdline nil))
		     (t
		      ;; Otherwise, that's really junk:
		      (push arg arglist)))))))
    (setf (arglist context) (nreverse arglist))))

;; #### FIXME: SBCL-specific
(defun make-context (&rest keys &key synopsis cmdline)
  "Make a new context.
- SYNOPSIS is the program synopsis to use in that context.
- CMDLINE is the argument list (strings) to process.
  It defaults to a POSIX conformant argv.
  The list is copied (the original is left untouched)."
  (declare (ignore synopsis cmdline))
  (apply #'make-instance 'context keys))


;; ============================================================================
;; The Option retrieval Protocol
;; ============================================================================

(defun getopt (context &rest keys &key short-name long-name option)
  "Get an option's value in CONTEXT.
The option can be specified either by SHORT-NAME, LONG-NAME, or directly via
an OPTION object."
  (unless option
    (setq option (apply #'search-option (synopsis context) keys)))
  (unless option
    (error "Getting option ~S from synopsis ~A in context ~A: option unknown."
	   (or short-name long-name)
	   (synopsis context)
	   context))
  (let ((minus-char (let ((mc (minus-char option)))
		      (when mc (coerce mc 'character))))
	(plus-char (let ((mc (plus-char option)))
		     (when mc (coerce mc 'character))))
	(arglist (list)))
    (do ((arg (pop (arglist context)) (pop (arglist context))))
	((null arg))
      ;; #### NOTE: actually, I *do* have a use for nreconc ;-)
      (cond ((and (cmdline-option-p arg) (eq (cmdline-option-option arg) option))
	     (setf (arglist context) (nreconc arglist (arglist context)))
	     (return-from getopt (convert option
				   (cmdline-option-name arg)
				   (cmdline-option-value arg))))
	    ((and (cmdline-pack-p arg)
		  (eq (cmdline-pack-type arg) :minus)
		  minus-char
		  (position minus-char (cmdline-pack-contents arg)))
	     (setf (cmdline-pack-contents arg)
		   (remove minus-char (cmdline-pack-contents arg)))
	     (setf (arglist context) (nreconc (push arg arglist) (arglist context)))
	     (return-from getopt (convert option (short-name option) "yes")))
	    ((and (cmdline-pack-p arg)
		  (eq (cmdline-pack-type arg) :plus)
		  plus-char
		  (position plus-char (cmdline-pack-contents arg)))
	     (setf (cmdline-pack-contents arg)
		   (remove plus-char (cmdline-pack-contents arg)))
	     (setf (arglist context) (nreconc (push arg arglist) (arglist context)))
	     (return-from getopt (convert option (short-name option) "no")))
	    (t
	     (push arg arglist))))
    (setf (arglist context) (nreverse arglist))))

;;; context.lisp ends here
