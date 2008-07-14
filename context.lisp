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
  name ;; the name as it appears on the command line
  option ;; the actual option is corresponds to, or null if unknown
  value ;; the option's value as it appears on the command line
  )

(defstruct cmdline-pack
  type ;; either :minus or :plus
  contents ;; the contents (characters) of the pack
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
  "Parse CMDLINE.
Extract program name, options, remainder and junk."
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
		   '(unless (option-p (car cmdline))
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
			(name (subseq arg 2 value-start))
			;; #### NOTE: we authorize partial matching
			;; (abreviations) for long names: an exact match is
			;; search first. If that fails, we try partial
			;; matching, and the first matching option is
			;; returned. For instance, if you have --foobar and
			;; --foo options in that order, passing --foo will
			;; match the option --foo, but passing --fo will match
			;; --foobar. I'm not sure this is the best behavior in
			;; such cases. Think harder about this.
			(option (or (search-option (synopsis context)
				      :long-name name)
				    (search-option (synopsis context)
				      :partial-name name)))
			(value (when value-start
				 (subseq arg (1+ value-start)))))
		   (cond ((eq (type-of option) 'flag) ;; a flag
			  ;; If we have an argument for a flag here (in case
			  ;; of --flag=nonsense), we register it so that the
			  ;; converter can later report the error. However, in
			  ;; case of "--flag nonsense", we don't consider the
			  ;; next cmdline item as a spurious arg. Rather, the
			  ;; next iteration will put it either into the junk
			  ;; category, or into the remainder.
			  (push (make-cmdline-option
				 :name (long-name option)
				 :option option
				 :value value)
				arglist))
			 (option ;; another (but then, valued) option
			  ;; If the option requires an argument, but none is
			  ;; provided by an =-syntax, we might find it in the
			  ;; next cmdline item, unless it looks like an option
			  ;; (but then, the converter will later report a
			  ;; missing argument error). Optional arguments are
			  ;; only available through the =-syntax, so we don't
			  ;; look into the next cmdline item. If the next
			  ;; cmdline item is not an option, the next iteration
			  ;; will put it either into the junk category, or
			  ;; into the remainder.
			  (push (make-cmdline-option
				 :name (long-name option)
				 :option option
				 :value (or value
					    (when (argument-required-p option)
					      (maybe-next-cmdline-arg))))
				arglist))
			 (t ;; an unknown option
			  ;; Unknown options are considered to require an
			  ;; argument, because that is the default. This
			  ;; choice leaves less junk on the cmdline.
			  (push (make-cmdline-option
				 :name name
				 :value (maybe-next-cmdline-arg))
				arglist)))))
		;; A short (possibly unknown) option or a minus pack:
		((string-start arg "-")
		 (let ((name (subseq arg 1))
		       option)
		   (cond ((setq option (search-option (synopsis context)
					 :short-name name))
			  (cond ((eq (type-of option) 'flag) ;; a flag
				 ;; In case of "-flag nonsense", we don't
				 ;; consider the next cmdline item as a
				 ;; spurious arg. Rather, the next iteration
				 ;; will put it either into the junk category,
				 ;; or into the remainder.
				 (push (make-cmdline-option
					:name (short-name option)
					:option option)
				       arglist))
				(option ;; another (but then, valued) option
				 ;; If the option requires an argument, it
				 ;; might be in the next cmdline item, unless
				 ;; it looks like an option (but then, the
				 ;; converter will later report a missing
				 ;; argument error). Optional arguments are
				 ;; necessary sticky, so we don't look into
				 ;; the next cmdline item. If this option's
				 ;; argument is optional and the next cmdline
				 ;; item is not an option, the next iteration
				 ;; will put it either into the junk category,
				 ;; or into the remainder.
				 (push (make-cmdline-option
					:name (short-name option)
					:option option
					:value (when (argument-required-p option)
						 (maybe-next-cmdline-arg)))
				       arglist))))
			 ((setq option (search-sticky-option (synopsis context)
							     name))
			  ;; We found an option with a sticky argument.
			  ;; #### NOTE: when looking for a sticky option, we
			  ;; stop at the first match, even if, for instance,
			  ;; another option would match a longer part of the
			  ;; argument. I'm not sure this is the best behavior
			  ;; in such cases. Think harder about this.
			  (push (make-cmdline-option
				 :name (short-name option)
				 :option option
				 :value (subseq name
						(length (short-name option))))
				arglist))
			 ((minus-pack (synopsis context))
			  ;; Let's find out whether this is a minus pack:
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
				   ;; (remember that those options don't
				   ;; appear in the minus pack description).
				   (setq option (search-option (synopsis context)
						  :short-name trimmed))
				   (cond ((and option
					       (argument-required-p option))
					  ;; We found an option. Separate the
					  ;; package into a simple minus pack,
					  ;; and a cmdline option:
					  (push
					   (make-cmdline-pack
					    :type :minus
					    :contents
					    (subseq name 0
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
					 ;; correspond to a known option
					 ;; requiring and argument. Consider
					 ;; the whole pack as an unknown
					 ;; option.
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
			  ;; switch. In case of a usage error (not a switch),
			  ;; we have a problem that might or might not be
			  ;; detected at retrieval time: flags would notice an
			  ;; extra value, a user converter might notice an
			  ;; invalid value, but a simple stropt would not
			  ;; notice anything. We could try to be more clever
			  ;; and detect that the option's type is wrong, but
			  ;; on the other hand, the +stropt is a funny trick
			  ;; to use even for non boolean options.
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
				    (make-cmdline-pack
				     :type :plus :contents name)
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
    (error "Getting option ~S from synopsis ~A in context ~A: option unknown."
	   (or short-name long-name)
	   (synopsis context)
	   context))
  (let ((minus-char (minus-char option))
	(plus-char (plus-char option))
	(arglist (list)))
    (do ((arg (pop (arglist context)) (pop (arglist context))))
	((null arg))
      ;; #### NOTE: actually, I *do* have a use for nreconc, he he ;-)
      (cond ((and (cmdline-option-p arg) (eq (cmdline-option-option arg) option))
	     (setf (arglist context) (nreconc arglist (arglist context)))
	     (return-from getopt (convert-value option
				   (cmdline-option-name arg)
				   (cmdline-option-value arg))))
	    ((and (cmdline-pack-p arg)
		  (eq (cmdline-pack-type arg) :minus)
		  minus-char
		  (position minus-char (cmdline-pack-contents arg)))
	     (setf (cmdline-pack-contents arg)
		   (remove minus-char (cmdline-pack-contents arg)))
	     (setf (arglist context) (nreconc (push arg arglist) (arglist context)))
	     (return-from getopt (convert-value option
				   (short-name option)
				   (unless (eq (type-of option) 'flag)
				     "yes"))))
	    ((and (cmdline-pack-p arg)
		  (eq (cmdline-pack-type arg) :plus)
		  plus-char
		  (position plus-char (cmdline-pack-contents arg)))
	     (setf (cmdline-pack-contents arg)
		   (remove plus-char (cmdline-pack-contents arg)))
	     (setf (arglist context) (nreconc (push arg arglist) (arglist context)))
	     (return-from getopt (convert-value option (short-name option) "no")))
	    (t
	     (push arg arglist))))
    (setf (arglist context) (nreverse arglist)))
  ;; Not found: try an env var
  (convert-environment option))

;;; context.lisp ends here
