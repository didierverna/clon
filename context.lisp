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
;; The Command-Line Structure
;; ============================================================================

(defstruct cmdline-option
  name ;; the option's name used on the cmdline
  option ;; the corresponding option object
  value ;; the corresponding converted value
  status ;; the status of the conversion
  )


;; ============================================================================
;; The Context Class
;; ============================================================================

;; #### FIXME: make final
(defclass context ()
  ((synopsis :documentation "The program synopsis."
	     :type synopsis
	     :reader synopsis
	     :initarg :synopsis)
   (progname :documentation
	     "The program name, as it appears on the command line."
	     :type string
	     :reader progname)
   (arglist :documentation "The argument list to process."
	    :type list
	    :accessor arglist)
   (remainder :documentation "The non-Clon part of the argument list."
	      :type list
	      :reader remainder)
   (junk :documentation "The unidentified part of the argument list."
	 :type list
	 :reader junk))
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
  "Parse CMDLINE."
  (declare (ignore synopsis))
  (setf (slot-value context 'progname) (pop cmdline))
  ;; ### FIXME: all the stuff below is not very well abstracted.
  ;; In order to parse the command line, we not only need a lexical and
  ;; syntactic analysis, but also a bit of semantics: indeed, the parsing
  ;; depends on the option type to some extent, and also on the argument
  ;; status (required or optional). The rest of the semantic analysis (most
  ;; notably value conversion) is done when options are actually retrieved.
  (let ((arglist (list))
	(remainder (list))
	(junk (list)))
    (flet ((option-p (arg)
	     (or (eq (elt arg 0) #\-)
		 (eq (elt arg 0) #\+))))
      (macrolet ((no-more-option ()
		   '(notany #'option-p cmdline))
		 (maybe-next-cmdline-arg ()
		   '(when (and (car cmdline) (not (option-p (car cmdline))))
		     (pop cmdline))))
	(do ((arg (pop cmdline) (pop cmdline)))
	    ((null arg))
	  (cond ((string= arg "--")
		 ;; The Clon separator:
		 ;; Isolate the rest of the command line.
		 (setq remainder cmdline)
		 (setq cmdline nil))
		;; A long (possibly unknown) option:
		((string-start arg "--")
		 (let* ((value-start (position #\= arg :start 2))
			(cmdline-name (subseq arg 2 value-start))
			(cmdline-value (when value-start
					 (subseq arg (1+ value-start))))
			(option (search-option (synopsis context)
				  :long-name cmdline-name)))
		   ;; #### NOTE: we authorize partial matching (abreviations)
		   ;; for long names: an exact match is search first. If that
		   ;; fails, we try partial matching, and the first matching
		   ;; option is returned. For instance, if you have --foobar
		   ;; and --foo options in that order, passing --foo will
		   ;; match the option --foo, but passing --fo will match
		   ;; --foobar. This is probably not the best behavior: it
		   ;; would be better to find the option "closest" to the
		   ;; partial match.
		   (unless option
		     (when (setq option (search-option (synopsis context)
					  :partial-name cmdline-name))
		       ;; The cmdline option's name is the long one, but in
		       ;; case of abbreviation (for instance --he instead of
		       ;; --help), we will display it like this: he(lp). In
		       ;; case of error report, this is closer to what the
		       ;; user actually typed.
		       (setq cmdline-name (complete-started-string
					   cmdline-name (long-name option)))))
		   (cond ((eq (type-of option) 'flag)
			  ;; This is a flag. Here, we can't avoid noticing
			  ;; when a flag was given an argument through an
			  ;; =-syntax. However, we don't check whether the
			  ;; next cmdline item could be a spurious arg,
			  ;; because that would mess with a possible automatic
			  ;; remainder detection. Rather, the next iteration
			  ;; will put it either into the junk category, or
			  ;; into the remainder.
			  (push (make-cmdline-option
				 :name cmdline-name
				 :option option
				 :value t
				 :status (if cmdline-value
					     (list :extra-argument cmdline-value)
					     t))
				arglist))
			 ((eq (type-of option) 'switch)
			  ;; This is a switch. The difference with other
			  ;; valued options (see below) is that an omitted
			  ;; optional argument stands for a "yes".
			  (cond ((argument-required-p option)
				 (unless cmdline-value
				   (setq cmdline-value
					 (maybe-next-cmdline-arg)))
				 (if cmdline-value
				     (destructuring-bind (value status)
					 (retrieve option cmdline-value)
				       (push (make-cmdline-option
					      :name cmdline-name
					      :option option
					      :value value
					      :status status)
					     arglist))
				     (push (make-cmdline-option
					    :name cmdline-name
					    :option option
					    :value (default-value option)
					    :status
					    (list :missing-argument))
					   arglist)))
				(t
				 (if cmdline-value
				     (destructuring-bind (value status)
					 (retrieve option cmdline-value)
				       (push (make-cmdline-option
					      :name cmdline-name
					      :option option
					      :value value
					      :status status)
					     arglist))
				     (push (make-cmdline-option
					    :name cmdline-name
					    :option option
					    :value t ;; omitted means "yes"
					    :status t)
					   arglist)))))
			 (option
			  ;; This is a valued option. If the option requires
			  ;; an argument, but none is provided by an =-syntax,
			  ;; we might find it in the next cmdline item, unless
			  ;; it looks like an option, in which case it is a
			  ;; missing argument error. Optional arguments are
			  ;; only available through the =-syntax, so we don't
			  ;; look into the next cmdline item. If the next
			  ;; cmdline item is not an option, the next iteration
			  ;; will put it either into the junk category, or
			  ;; into the remainder.
			  (cond ((argument-required-p option)
				 (unless cmdline-value
				   (setq cmdline-value
					 (maybe-next-cmdline-arg)))
				 (if cmdline-value
				     (destructuring-bind (value status)
					 (retrieve option cmdline-value)
				       (push (make-cmdline-option
					      :name cmdline-name
					      :option option
					      :value value
					      :status status)
					     arglist))
				     (push (make-cmdline-option
					    :name cmdline-name
					    :option option
					    :value (default-value option)
					    :status
					    (list :missing-argument))
					   arglist)))
				(t
				 (if cmdline-value
				     (destructuring-bind (value status)
					 (retrieve option cmdline-value)
				       (push (make-cmdline-option
					      :name cmdline-name
					      :option option
					      :value value
					      :status status)
					     arglist))
				     (push (make-cmdline-option
					    :name cmdline-name
					    :option option
					    :value (default-value option)
					    :status t)
					   arglist)))))
			 (t
			  ;; This is an unknown option. These ones are
			  ;; considered to require an argument, because that
			  ;; is the default in the valued-option abstract
			  ;; class. This choice leaves less junk on the
			  ;; cmdline. A missing argument to an unknown option
			  ;; is not reported though (things are messed up well
			  ;; enough already).
			  (push (make-cmdline-option
				 :name cmdline-name
				 :value (or cmdline-value
					    (maybe-next-cmdline-arg)))
				arglist)))))
		;; A short (possibly unknown) option or a minus pack:
		((string-start arg "-")
		 ;; #### FIXME: check invalid syntax -foo=val
		 (let ((cmdline-name (subseq arg 1))
		       option)
		   ;; #### NOTE: we don't allow partial match on short names
		   ;; because that would make it too complicated to
		   ;; distinguish abreviations, sticky arguments and stuff.
		   (cond ((setq option (search-option (synopsis context)
					 :short-name cmdline-name))
			  (cond ((or (eq (type-of option) 'flag)
				     (eq (type-of option) 'switch))
				 ;; If this is a flag, we don't check whether
				 ;; the next cmdline item could be a spurious
				 ;; arg, because that would mess with a
				 ;; possible automatic remainder detection.
				 ;; Rather, the next iteration will put it
				 ;; either into the junk category, or into the
				 ;; remainder.
				 ;;
				 ;; If this is a switch, it doesn't take any
				 ;; argument in short form (whether optional
				 ;; or not), so we don't check anything
				 ;; either. the minus form just means "yes".
				 (push (make-cmdline-option
					:name cmdline-name
					:option option
					:value t
					:status t)
				       arglist))
				(option
				 ;; This is a valued option. If the option
				 ;; requires an argument, we might find it in
				 ;; the next cmdline item, unless it looks
				 ;; like an option, in which case it is a
				 ;; missing argument error. Optional arguments
				 ;; are necessarily sticky, so we don't look
				 ;; into the next cmdline item. If the next
				 ;; cmdline item is not an option, the next
				 ;; iteration will put it either into the junk
				 ;; category, or into the remainder.
				 (cond ((argument-required-p option)
					(let ((cmdline-value
					       (maybe-next-cmdline-arg)))
					  (if cmdline-value
					      (destructuring-bind (value status)
						  (retrieve option cmdline-value)
						(push (make-cmdline-option
						       :name cmdline-name
						       :option option
						       :value value
						       :status status)
						      arglist))
					      (push (make-cmdline-option
						     :name cmdline-name
						     :option option
						     :value
						     (default-value option)
						     :status
						     (list :missing-argument))
						    arglist))))
				       (t
					(push (make-cmdline-option
					       :name cmdline-name
					       :option option
					       :value (default-value option)
					       :status t)
					      arglist))))))
			 ((setq option (search-sticky-option
					(synopsis context) cmdline-name))
			  ;; We found an option with a sticky argument.
			  ;; #### NOTE: when looking for a sticky option, we
			  ;; stop at the first match, even if, for instance,
			  ;; another option would match a longer part of the
			  ;; argument. This is probably not the best behavior:
			  ;; it would be better to find the option "closest"
			  ;; to the partial match.
			  (destructuring-bind (value status)
			      (retrieve option
				(subseq cmdline-name
					(length (short-name option))))
			    (push (make-cmdline-option
				   :name (short-name option)
				   :option option
				   :value value
				   :status status)
				  arglist)))
			 ((minus-pack (synopsis context))
			  ;; Let's find out whether this is a minus pack:
			  ;; #### NOTE: the way things are currently designed,
			  ;; only conformant options return a minus char. This
			  ;; means that if a non-conformant option appears by
			  ;; mistake in a minus pack, Clon won't detect that,
			  ;; but will detect an unknown option instead.
			  (let ((trimmed (string-left-trim
					  (minus-pack (synopsis context))
					  cmdline-name)))
			    (cond ((zerop (length trimmed))
				   ;; We found a simple minus pack. Remember
				   ;; that it is composed of flags, switches,
				   ;; or options with optional argument (not
				   ;; given here). Split the pack into
				   ;; multiple option calls.
				   (loop :for char :across cmdline-name
					 :do
					 (let* ((name (make-string
						       1
						       :initial-element char))
						(option (search-option
							    (synopsis context)
							  :short-name
							  name)))
					   (assert option)
					   (push (make-cmdline-option
						  :name name
						  :option option
						  :value
						  (if (or (eq (type-of option)
							      'flag)
							  (eq (type-of option)
							      'switch))
						      t
						      (default-value option))
						  :status t)
						 arglist))))
				  ((= (length trimmed) 1)
				   ;; There's one character left: this can be
				   ;; an option requiring an argument, with
				   ;; the argument in the next cmdline item.
				   ;; (remember that those options don't
				   ;; appear in the minus pack description).
				   (setq option (search-option (synopsis context)
						  :short-name trimmed))
				   (cond (option
					  ;; We found an option. Split the
					  ;; pack into multiple option calls,
					  ;; including the last one which is a
					  ;; bit different.
					  (assert (argument-required-p option))
					  ;; (otherwise, this option would
					  ;; have appeared in the pack)
					  (loop :for char
					    :across
					    (subseq cmdline-name 0
						    (1- (length
							 cmdline-name)))
					    :do
					    (let* ((name
						    (make-string
						     1
						     :initial-element char))
						   (option (search-option
							       (synopsis
								context)
							     :short-name
							     name)))
					      (assert option)
					      (push (make-cmdline-option
						     :name name
						     :option option
						     :value
						     (if (or (eq (type-of
								  option)
								 'flag)
							     (eq (type-of
								  option)
								 'switch))
							 t
							 (default-value option))
						     :status t)
						    arglist)))
					  (let ((cmdline-value
						 (maybe-next-cmdline-arg)))
					    (if cmdline-value
						(destructuring-bind
						      (value status)
						    (retrieve option
						      cmdline-value)
						  (push (make-cmdline-option
							 :name trimmed
							 :option option
							 :value value
							 :status status)
							arglist))
						(push (make-cmdline-option
						       :name trimmed
						       :option option
						       :value (default-value
								  option)
						       :status
						       (list
							:missing-argument))
						      arglist))))
					 ;; The last character doesn't
					 ;; correspond to a known option.
					 ;; Consider the whole pack as an
					 ;; unknown option.
					 (t
					  (push (make-cmdline-option
						 :name cmdline-name
						 :value
						 (maybe-next-cmdline-arg))
						arglist))))
				  (t
				   ;; There's more than one character left. As
				   ;; above, consider the whole pack as an
				   ;; unknown option.
				   (push (make-cmdline-option
					  :name cmdline-name
					  :value (maybe-next-cmdline-arg))
					 arglist))))))))
		;; A short (possibly unknown) switch, or a plus pack.
		((string-start arg "+")
		 ;; #### FIXME: check invalid syntax +foo=val
		 (let ((cmdline-name (subseq arg 1))
		       option)
		   (cond ((setq option (search-option (synopsis context)
					 :short-name cmdline-name))
			  ;; We found an option.
			  (push (make-cmdline-option
				 :name cmdline-name
				 :option option
				 :value (etypecase option
					  (flag t)
					  (switch nil)
					  (t (default-value option)))
				 :status (if (eq (type-of option) 'switch)
					     t
					     (list :invliad-+-syntax)))
				arglist))
			 ((plus-pack (synopsis context))
			  ;; Let's find out whether it is a plus pack:
			  (let ((trimmed (string-left-trim
					  (plus-pack (synopsis context))
					  cmdline-name)))
			    (cond ((zerop (length trimmed))
				   ;; We found a plus pack. Split the pack
				   ;; into multiple option calls.
				   (loop :for char :across cmdline-name
					 :do
					 (let* ((name (make-string
						       1
						       :initial-element char))
						(option (search-option
							    (synopsis context)
							  :short-name
							  name)))
					   (assert option)
					   (assert (eq (type-of option)
						       'switch))
					   (push (make-cmdline-option
						  :name name
						  :option option
						  :value nil
						  :status t)
						 arglist))))
				  (t
				   ;; We found an unknown switch:
				   (push
				    (make-cmdline-option :name cmdline-name
							 :value "no")
				    arglist))))))))
		;; Otherwise, it's junk.
		(t
		 ;; #### FIXME: SBCL specific.
		 (cond ((sb-ext:posix-getenv "POSIXLY_CORRECT")
			;; That's the end of the Clon-specific part:
			(setq remainder (cons arg cmdline))
			(setq cmdline nil))
		       (t
			;; If there's no more option on the cmdline, consider
			;; this as the remainder (implicit since no "--" has
			;; been used). If there's still another option
			;; somewhere, then this is really junk.
			(cond ((no-more-option)
			       (setq remainder (cons arg cmdline))
			       (setq cmdline nil))
			      (t
			       (push arg junk))))))))))
    (setf (arglist context) (nreverse arglist))
    (setf (slot-value context 'remainder) remainder)
    (setf (slot-value context 'junk) junk)))

;; #### FIXME: SBCL-specific
(defun make-context (&rest keys &key synopsis cmdline)
  "Make a new context.
- SYNOPSIS is the program synopsis to use in that context.
- CMDLINE is the argument list (strings) to process.
  It defaults to a POSIX conformant argv."
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
    (error "Getting option ~S from synopsis ~A in context ~A: unknown option."
	   (or short-name long-name)
	   (synopsis context)
	   context))
  (let ((arglist (list)))
    (do ((arg (pop (arglist context)) (pop (arglist context))))
	((null arg))
      ;; #### NOTE: actually, I *do* have a use for nreconc, he he ;-)
      (cond ((and (cmdline-option-p arg) (eq (cmdline-option-option arg) option))
	     (setf (arglist context) (nreconc arglist (arglist context)))
	     (return-from getopt (values (cmdline-option-value arg)
					 (if (eq (cmdline-option-status arg)
						 t)
					     t
					     (cons (cmdline-option-name arg)
						   (cmdline-option-status
						    arg)))
					 :cmdline)))
	    (t
	     (push arg arglist))))
    (setf (arglist context) (nreverse arglist)))
  ;; Not found: try an env var
  ;; #### FIXME: SBCL-specific
  (let ((value (sb-posix:getenv (env-var option))))
    (if value
	(cond ((eq (type-of option) 'flag)
	       (return-from getopt
		 (values t t (list :environment (env-var option)))))
	      (t
	       (destructuring-bind (value status) (retrieve option value)
		 (if (eq status t)
		     (return-from getopt
		       (values value t (list :environment (env-var option))))
		     (return-from getopt
		       (values (default-value option)
			       (cons (or (long-name option)
					 (short-name option))
				     status)
			       (list :environment (env-var option))))))))
	(return-from getopt (values (if (eq (type-of option) 'flag)
					nil
					(default-value option))
				    nil)))))

;;; context.lisp ends here
