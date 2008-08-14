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

(defstruct unknown-option
  name ;; the option's name as used on the cmdline
  value ;; the option's value, as appearing on the cmdline
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
   (cmdline-options :documentation "The options on the command line."
	  :type list
	  :accessor cmdline-options)
   (remainder :documentation "The non-Clon part of the command line."
	      :type list
	      :reader remainder)
   (unknown-options :documentation "The unknown options on the command line."
		    :type list
		    :accessor unknown-options)
   (junk :documentation "The unidentified part of the command line."
	 :type list
	 :accessor junk))
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
  (let ((cmdline-options (list))
	(remainder (list))
	(unknown-options (list))
	(junk (list)))
    (macrolet ((push-cmdline-option (place &rest body)
		 "Push a new CMDLINE-OPTION created with BODY onto PLACE."
		 `(push (make-cmdline-option ,@body) ,place))
	       (push-unknown-option (place &rest body)
		 "Push a new UNKNOWN-OPTION created with BODY onto PLACE."
		 `(push (make-unknown-option ,@body) ,place))
	       (push-retrieved-option
		   (place func option &optional cmdline-value cmdline name-form)
		   "Retrieve OPTION from a FUNC call and push it onto PLACE.
- FUNC must be either :long, :short or :plus,
- CMDLINE-VALUE is a potentially already parsed option argument,
- CMDILNE is where to find a potentially required argument,
- NAME-FORM is how to compute the :name slot of the CMDLINE-OPTION structure.
  If not given, the option's long or short name will be used as appropriate."
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
		     (when (eq func :long)
		       (push name-form call))
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
		       (push-cmdline-option ,place
			 :name ,name-form
			 :option ,option
			 :value ,value
			 :status ,status))))
	       (do-pack ((option pack context) &body body)
		 "Evaluate BODY with OPTION bound to each option from PACK.
CONTEXT is where to look for the options."
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
	       ;; The Clon separator.
	       (setq remainder cmdline)
	       (setq cmdline nil))
	      ((string-start arg "--")
	       ;; A long call.
	       (let* ((value-start (position #\= arg :start 2))
		      (cmdline-name (subseq arg 2 value-start))
		      (cmdline-value (when value-start
				       (subseq arg (1+ value-start))))
		      (option (search-option context :long-name cmdline-name))
		      (name cmdline-name))
		 (unless option
		   (multiple-value-setq (option name)
		     (search-option context :partial-name cmdline-name)))
		 (if option
		     (push-retrieved-option cmdline-options :long option
		       cmdline-value cmdline name)
		     (push-unknown-option unknown-options
		       :name cmdline-name
		       :value cmdline-value))))
	      ;; A short call, or a minus pack.
	      ((string-start arg "-")
	       ;; #### FIXME: check invalid syntax -foo=val
	       (let* ((cmdline-name (subseq arg 1))
		      (option (search-option context :short-name cmdline-name))
		      cmdline-value)
		 (unless option
		   (multiple-value-setq (option cmdline-value)
		     (search-sticky-option context cmdline-name)))
		 (cond (option
			(push-retrieved-option cmdline-options :short option
			  cmdline-value cmdline))
		       ((potential-pack-p cmdline-name context)
			;; #### NOTE: When parsing a minus pack, only the last
			;; option gets a cmdline argument because only the
			;; last one is allowed to retrieve an argument from
			;; there.
			(do-pack (option
				  (subseq cmdline-name 0
					  (1- (length cmdline-name)))
				  context)
			  (push-retrieved-option cmdline-options :short option))
			(let* ((name (subseq cmdline-name
					     (1- (length cmdline-name))))
			       (option (search-option context :short-name name)))
			  (assert option)
			  (push-retrieved-option cmdline-options :short option
			    nil cmdline)))
		       (t
			(push-unknown-option unknown-options
			  :name cmdline-name)))))
	      ;; A plus call or a plus pack.
	      ((string-start arg "+")
	       ;; #### FIXME: check invalid syntax +foo=val
	       (let* ((cmdline-name (subseq arg 1))
		      ;; #### NOTE: in theory, we could allow partial matches
		      ;; on short names when they're used with the +-syntax,
		      ;; because there's no sticky argument or whatever. But
		      ;; we don't. That's all. Short names are not meant to be
		      ;; long (otherwise, that would be long names right?), so
		      ;; they're not meant to be abbreviated.
		      (option (search-option context :short-name cmdline-name)))
		 (cond (option
			(push-retrieved-option cmdline-options :plus option))
		       ((potential-pack-p cmdline-name context)
			(do-pack (option cmdline-name context)
			  (push-retrieved-option cmdline-options :plus option)))
		       (t
			(push-unknown-option unknown-options
			  :name cmdline-name)))))
	      (t
	       ;; Not an option call.
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
      (setf (cmdline-options context) (nreverse cmdline-options))
      (setf (slot-value context 'remainder) remainder)
      (setf (unknown-options context) (nreverse unknown-options))
      (setf (slot-value context 'junk) (nreverse junk)))))

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
;; The Option Retrieval Protocol
;; ============================================================================

(defun getopt (context &rest keys &key short-name long-name option)
  "Get an option's value in CONTEXT.
The option can be specified either by SHORT-NAME, LONG-NAME, or directly via
an OPTION object.
This function returns three values:
- the retrieved value,
- the retrieval status,
- the value's source."
  (unless option
    (setq option (apply #'search-option context keys)))
  (unless option
    (error "Getting option ~S from synopsis ~A in context ~A: unknown option."
	   (or short-name long-name)
	   (synopsis context)
	   context))
  ;; Try the command line:
  (let ((cmdline-options (list)))
    (do ((cmdline-option
	  (pop (cmdline-options context))
	  (pop (cmdline-options context))))
	((null cmdline-option))
      ;; #### NOTE: actually, I *do* have a use for nreconc, he he ;-)
      (cond ((eq (cmdline-option-option cmdline-option) option)
	     (setf (cmdline-options context)
		   (nreconc cmdline-options (cmdline-options context)))
	     (return-from getopt
	       (values (cmdline-option-value cmdline-option)
		       (or (eq (cmdline-option-status cmdline-option) t)
			   (list* (cmdline-option-option cmdline-option)
				  (cmdline-option-status cmdline-option)))
		       (list :cmdline (cmdline-option-name cmdline-option)))))
	    (t
	     (push cmdline-option cmdline-options))))
    (setf (cmdline-options context) (nreverse cmdline-options)))
  ;; Otherwise, fallback to the environment or a default value:
  (fallback-retrieval option))

(defun getopt-cmdline (context)
  "Get the next cmdline option in CONTEXT.
This function returns four values:
- the option object,
- the option's name used on the command line,
- the retrieved value,
- the retrieval status."
  (let ((cmdline-option (pop (cmdline-options context))))
    (when cmdline-option
      (values (cmdline-option-option cmdline-option)
	      (cmdline-option-name cmdline-option)
	      (cmdline-option-value cmdline-option)
	      (cmdline-option-status cmdline-option)))))

(defmacro multiple-value-getopt-cmdline
    ((option name value status) context &body body)
  "Evaluate BODY on the next command line option.
OPTION, NAME, VALUE and STATUS are bound to the option's object, name used on
the command line), retrieved value and retrieval status."
  `(multiple-value-bind (,option ,name ,value ,status)
    (getopt-cmdline ,context)
    ,@body))

(defmacro do-cmdline-options ((option name value status) context &body body)
  "Evaluate BODY over all command line options from CONTEXT.
OPTION, NAME, VALUE and STATUS are bound to each option's object, name used on
the command line), retrieved value and retrieval status."
  `(do () ((null (cmdline-options ,context)))
    (multiple-value-getopt-cmdline (,option ,name ,value ,status) ,context
     ,@body)))


;; ============================================================================
;; The Unknown Option Protocol
;; ============================================================================

(defun getopt-unknown (context)
  "Get the next unknown option in CONTEXT.
This function returns two values:
- the option's name used on the command line,
- possibly a provided argument."
  (let ((unknown-option (pop (unknown-options context))))
    (when unknown-option
      (values (unknown-option-name unknown-option)
	      (unknown-option-value unknown-option)))))

(defmacro multiple-value-getopt-unknown
    ((name value) context &body body)
  "Evaluate BODY on the next unknown option.
NAME and VALUE are bound to the option's name used on the command line, and
possibly a provided value."
  `(multiple-value-bind (,name ,value)
    (getopt-unknown ,context)
    ,@body))

(defmacro do-unknown-options ((name value) context &body body)
  "Evaluate BODY over all unknown options from CONTEXT.
NAME and VALUE are bound to each option's name used on the command line, and
possibly a provided value."
  `(do () ((null (unknown-options ,context)))
    (multiple-value-getopt-unknown (,name ,value) ,context
     ,@body)))


;;; context.lisp ends here
