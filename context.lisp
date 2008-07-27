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
;; The Command Line Option Structure
;; ============================================================================

(defstruct cmdline-option
  name ;; the option's name as used on the cmdline
  option ;; the corresponding option object
  value ;; the converted option's cmdline value
  status ;; the conversion status
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
    (do ((arg (pop cmdline) (pop cmdline)))
	((null arg))
      (cond ((string= arg "--")
	     ;; The Clon separator: isolate the rest of the command line.
	     (setq remainder cmdline)
	     (setq cmdline nil))
	    ;; A long (possibly unknown) option:
	    ((string-start arg "--")
	     (let* ((value-start (position #\= arg :start 2))
		    (cmdline-name (subseq arg 2 value-start))
		    (cmdline-value (when value-start
				     (subseq arg (1+ value-start))))
		    ;; #### NOTE: we authorize partial matching (abreviations)
		    ;; for long names: an exact match is search first. If that
		    ;; fails, we try partial matching, and the first matching
		    ;; option is returned. For instance, if you have --foobar
		    ;; and --foo options in that order, passing --foo will
		    ;; match the option --foo, but passing --fo will match
		    ;; --foobar. This is probably not the best behavior: it
		    ;; would be better to find the option "closest" to the
		    ;; partial match.
		    (option (or (search-option (synopsis context)
				  :long-name cmdline-name)
				(search-option (synopsis context)
				  :partial-name cmdline-name))))
	       (if option
		   ;; We have an option. Let's retrieve its actual value.
		   (destructuring-bind (new-cmdline value status)
		       (retrieve-from-long-call option cmdline cmdline-value)
		     (setq cmdline new-cmdline)
		     (push (make-cmdline-option
			    ;; The cmdline option's name is the long one, but
			    ;; in case of abbreviation (for instance --he
			    ;; instead of --help), we will display it like
			    ;; this: he(lp). In case of error report, this
			    ;; will help the user spot where he did something
			    ;; wrong.
			    :name
			    (complete-string cmdline-name (long-name option))
			    :option option
			    :value value
			    :status status)
			   arglist))
		   ;; We have an unknown option. Don't mess with the rest of
		   ;; the cmdline in order to avoid conflict with the
		   ;; automatic remainder detection. Of course, the next
		   ;; cmdline item might be an argument for a misspelled
		   ;; option, but when things are messed up, they're messed
		   ;; up, that's all.
		   (push (make-cmdline-option :name cmdline-name
					      :value cmdline-value)
			 arglist))))
	    ;; A short (possibly unknown) option or a minus pack:
	    ((string-start arg "-")
	     ;; #### FIXME: check invalid syntax -foo=val
	     (let* ((cmdline-name (subseq arg 1))
		    ;; #### NOTE: we don't allow partial match on short names
		    ;; because that would make it too complicated to
		    ;; distinguish abreviations, sticky arguments and stuff.
		    (option (or (search-option (synopsis context)
				  :short-name cmdline-name)
				;; #### NOTE: when looking for a sticky
				;; option, we stop at the first match, even
				;; if, for instance, another option would
				;; match a longer part of the argument. This
				;; is probably not the best behavior: it would
				;; be better to find the option "closest" to
				;; the partial match.
				(search-sticky-option
				 (synopsis context) cmdline-name))))
	       (cond (option
		      ;; We have an option. Let's retrieve its actual value,
		      ;; maybe already with a sticky argument:
		      (let ((cmdline-value
			     (when (not (string= cmdline-name
						 (short-name option)))
			       (subseq cmdline-name
				       (length (short-name option))))))
			(destructuring-bind (new-cmdline value status)
			    (retrieve-from-short-call option cmdline
						      cmdline-value)
			  (setq cmdline new-cmdline)
			  (push (make-cmdline-option
				 :name (short-name option)
				 :option option
				 :value value
				 :status status)
				arglist))))
		     ((potential-pack (synopsis context))
		      ;; Let's find out whether this is a minus pack:
		      (let ((trimmed (string-left-trim
				      (potential-pack (synopsis context))
				      cmdline-name)))
			(cond ((zerop (length trimmed))
			       ;; We found a potential minus pack. Split it
			       ;; into multiple short calls. Only the last one
			       ;; gets a cmdline, though, because only the
			       ;; last one is allowed to get an argument from
			       ;; the next cmdline arg.
			       (loop :for char
				 :across (subseq cmdline-name
						 0
						 (1- (length cmdline-name)))
				 :do
				 (let* ((name (make-string 1
						:initial-element char))
					(option (search-option (synopsis context)
						  :short-name name)))
				   (assert option)
				   (destructuring-bind (new-cmdline value status)
				       (retrieve-from-short-call option nil)
				     (declare (ignore new-cmdline))
				     (push (make-cmdline-option
					    :name (short-name option)
					    :option option
					    :value value
					    :status status)
					   arglist))))
			       (let* ((name (subseq cmdline-name
						    (1-
						     (length cmdline-name))))
				      (option (search-option (synopsis context)
						:short-name name)))
				 (assert option)
				 (destructuring-bind (new-cmdline value status)
				     (retrieve-from-short-call option cmdline)
				   (setq cmdline new-cmdline)
				   (push (make-cmdline-option
					  :name (short-name option)
					  :option option
					  :value value
					  :status status)
					 arglist))))
			      (t
			       ;; This is not a minus pack, so we have an
			       ;; unknown option. Don't mess with the rest of
			       ;; the cmdline in order to avoid conflict with
			       ;; the automatic remainder detection. Of
			       ;; course, the next cmdline item might be an
			       ;; argument for a misspelled option, but when
			       ;; things are messed up, they're messed up,
			       ;; that's all.
			       (push (make-cmdline-option :name cmdline-name)
				     arglist)))))
		     (t
		      ;; There's no potential pack, so we have an unknown
		      ;; option. Don't mess with the rest of the cmdline in
		      ;; order to avoid conflict with the automatic remainder
		      ;; detection. Of course, the next cmdline item might be
		      ;; an argument for a misspelled option, but when things
		      ;; are messed up, they're messed up, that's all.
		      (push (make-cmdline-option :name cmdline-name)
			    arglist)))))
	    ;; A short (possibly unknown) switch, or a plus pack.
	    ((string-start arg "+")
	     ;; #### FIXME: check invalid syntax +foo=val
	     (let* ((cmdline-name (subseq arg 1))
		    ;; #### NOTE: in theory, we could allow partial matches on
		    ;; short names when they're used with the +-syntax,
		    ;; because there's no sticky argument or whatever. But we
		    ;; don't. That's all. Short names are not meant to be long
		    ;; (otherwise, that would be long names right ?), so
		    ;; they're not meant to be abbreviated.
		    (option (search-option (synopsis context)
			      :short-name cmdline-name)))
	       (cond (option
		      ;; We found an option.
		      (destructuring-bind (value status)
			  (retrieve-from-plus-call option)
			(push (make-cmdline-option
			       :name (short-name option)
			       :option option
			       :value value
			       :status status)
			      arglist)))
		     ((potential-pack (synopsis context))
		      ;; Let's find out whether this is a plus pack:
		      (let ((trimmed (string-left-trim
				      (potential-pack (synopsis context))
				      cmdline-name)))
			(if (zerop (length trimmed))
			    ;; We found a potential plus pack. Split the pack
			    ;; into multiple option calls.
			    (loop :for char :across cmdline-name
				  :do
				  (let* ((name (make-string 1
						 :initial-element char))
					 (option (search-option
						     (synopsis context)
						   :short-name name)))
				    (destructuring-bind (value status)
					(retrieve-from-plus-call option)
				      (push (make-cmdline-option
					     :name (short-name option)
					     :option option
					     :value value
					     :status status)
					    arglist))))
			    ;; This is not a plus pack, so we have an unknown
			    ;; option.
			    (push (make-cmdline-option :name cmdline-name)
				  arglist))))
		     (t
		      ;; There's no potential pack, so we have an unknown
		      ;; option.
		      (push (make-cmdline-option :name cmdline-name)
			    arglist)))))
	    ;; Otherwise, it's junk.
	    (t
	     ;; #### FIXME: SBCL specific.
	     (cond ((sb-ext:posix-getenv "POSIXLY_CORRECT")
		    ;; That's the end of the Clon-specific part:
		    (setq remainder (cons arg cmdline))
		    (setq cmdline nil))
		   (t
		    ;; If there's no more option on the cmdline, consider this
		    ;; as the remainder (implicit since no "--" has been
		    ;; used). If there's still another option somewhere, then
		    ;; this is really junk.
		    (cond ((notany #'option-p cmdline)
			   (setq remainder (cons arg cmdline))
			   (setq cmdline nil))
			  (t
			   (push arg junk))))))))
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
