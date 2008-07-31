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
  (let ((arglist (list))
	(remainder (list))
	(junk (list)))
    (macrolet ((push-cmdline-option (arglist &rest body)
		 `(push (make-cmdline-option ,@body) ,arglist))
	       (push-retrieved-option
		   (func arglist option &optional cmdline-value cmdline name-form)
		   (let* ((value (gensym "value"))
			  (status (gensym "status"))
			  (vars (list status value))
			  (call (list option
				      (find-symbol (concatenate 'string
						     "RETRIEVE-FROM-"
						     (symbol-name func)
						     "-CALL")
						   'clon)))
			  new-cmdline)
		     (unless name-form
		       (setq name-form
			     (ecase func
			       (:long `(long-name ,option))
			       (:short `(short-name ,option))
			       (:plus `(short-name ,option)))))
		     (when cmdline-value
		       (push cmdline-value call))
		     (when cmdline
		       (setq new-cmdline (gensym "new-cmdline"))
		       (push new-cmdline vars)
		       (unless cmdline-value
			 (push nil call))
		       (push cmdline call))
		     `(multiple-value-bind ,(reverse vars) ,(reverse call)
		       ,(when cmdline `(setq ,cmdline ,new-cmdline))
		       (push-cmdline-option ,arglist
			 :name ,name-form
			 :option ,option
			 :value ,value
			 :status ,status))))
	       (do-pack ((option pack context) &body body)
		 (let ((char (gensym "char"))
		       (name (gensym "name")))
		   `(loop :for ,char :across ,pack
		     :do (let* ((,name (make-string 1 :initial-element ,char))
				(,option (search-option ,context
					   :short-name ,name)))
			   (assert ,option)
			   ,@body)))))
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
		      (option (or (search-option context
				    :long-name cmdline-name)
				  (search-option context
				    :partial-name cmdline-name))))
		 (if option
		     ;; We found an option. The cmdline option's name is the
		     ;; long one, but in case of abbreviation (for instance
		     ;; --he instead of --help), we will register it like
		     ;; this: he(lp). In case of error report, this will help
		     ;; the user spot where he did something wrong.
		     (push-retrieved-option :long arglist option
		       cmdline-value cmdline
		       (complete-string cmdline-name (long-name option)))
		     ;; We have an unknown option. Don't mess with the rest of
		     ;; the cmdline in order to avoid conflict with the
		     ;; automatic remainder detection. Of course, the next
		     ;; cmdline item might be an argument for a misspelled
		     ;; option, but when things are messed up, they're messed
		     ;; up, that's all.
		     (push-cmdline-option arglist
		       :name cmdline-name
		       :value cmdline-value))))
	      ;; A short (possibly unknown) option or a minus pack:
	      ((string-start arg "-")
	       ;; #### FIXME: check invalid syntax -foo=val
	       (let* ((cmdline-name (subseq arg 1))
		      (option (or (search-option context
				    :short-name cmdline-name)
				  ;; #### FIXME: have search-sticky-option
				  ;; return 2 values: the option and the
				  ;; argument.
				  (search-sticky-option context cmdline-name))))
		 (cond (option
			;; We have an option. Let's retrieve its actual value,
			;; maybe already with a sticky argument:
			(let ((cmdline-value
			       (when (not (string= cmdline-name
						   (short-name option)))
				 (subseq cmdline-name
					 (length (short-name option))))))
			  (push-retrieved-option :short arglist option
			    cmdline-value cmdline)))
		       ((potential-pack-p cmdline-name context)
			;; We found a potential minus pack. Split it into
			;; multiple short calls. Only the last one gets a
			;; cmdline, though, because only the last one is
			;; allowed to get an argument from the next cmdline
			;; arg.
			(do-pack (option (subseq cmdline-name
						 0 (1- (length cmdline-name)))
					 context)
			  (push-retrieved-option :short arglist option))
			(let* ((name (subseq cmdline-name
					     (1- (length cmdline-name))))
			       (option (search-option context :short-name name)))
			  (assert option)
			  (push-retrieved-option :short arglist option
			    nil cmdline)))
		       (t
			;; This is not a minus pack (or there is none), so we
			;; have an unknown option. Don't mess with the rest of
			;; the cmdline in order to avoid conflict with the
			;; automatic remainder detection. Of course, the next
			;; cmdline item might be an argument for a misspelled
			;; option, but when things are messed up, they're
			;; messed up, that's all.
			(push-cmdline-option arglist :name cmdline-name)))))
	      ;; A short (possibly unknown) switch, or a plus pack.
	      ((string-start arg "+")
	       ;; #### FIXME: check invalid syntax +foo=val
	       (let* ((cmdline-name (subseq arg 1))
		      ;; #### NOTE: in theory, we could allow partial matches
		      ;; on short names when they're used with the +-syntax,
		      ;; because there's no sticky argument or whatever. But
		      ;; we don't. That's all. Short names are not meant to be
		      ;; long (otherwise, that would be long names right ?),
		      ;; so they're not meant to be abbreviated.
		      (option (search-option context :short-name cmdline-name)))
		 (cond (option
			;; We found an option.
			(push-retrieved-option :plus arglist option))
		       ((potential-pack-p cmdline-name context)
			;; We found a potential plus pack. Split the pack into
			;; multiple option calls.
			(do-pack (option cmdline-name context)
			  (push-retrieved-option :plus arglist option))
			;; This is not a plus pack (or there is none), so we
			;; have an unknown option.
			(push-cmdline-option arglist :name cmdline-name)))))
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
		      (cond ((notany #'option-p cmdline)
			     (setq remainder (cons arg cmdline))
			     (setq cmdline nil))
			    (t
			     (push arg junk))))))))
      (setf (arglist context) (nreverse arglist))
      (setf (slot-value context 'remainder) remainder)
      (setf (slot-value context 'junk) junk))))

;; #### FIXME: SBCL-specific
(defun make-context (&rest keys &key synopsis cmdline)
  "Make a new context.
- SYNOPSIS is the program synopsis to use in that context.
- CMDLINE is the argument list (strings) to process.
  It defaults to a POSIX conformant argv."
  (declare (ignore synopsis cmdline))
  (apply #'make-instance 'context keys))


;; -----------------------
;; Potential pack protocol
;; -----------------------

(defmethod potential-pack-p (pack (context context))
  "Return t if PACK (a string) is a potential pack in CONTEXT."
  (potential-pack-p pack (synopsis context)))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod search-option
    ((context context) &rest keys &key short-name long-name partial-name)
  "Search for option in CONTEXT."
  (declare (ignore short-name long-name partial-name))
  (apply #'search-option (synopsis context) keys))

(defmethod search-sticky-option ((context context) namearg)
  "Search for a sticky option in CONTEXT."
  (search-sticky-option (synopsis context) namearg))


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
