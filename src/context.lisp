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
(in-readtable :clon)


;; ============================================================================
;; The Command-Line Items
;; ============================================================================

(defstruct cmdline-option
  name ;; the option's name as used on the cmdline
  option ;; the corresponding option object
  value ;; the converted option's cmdline value
  )

(define-condition invalid--=-syntax (cmdline-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Invalid =-syntax in short call: ~S."
	       (item error))))
  (:documentation "An error related to a -= syntax."))

(define-condition invalid-+=-syntax (cmdline-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Invalid =-syntax in plus call: ~S."
	       (item error))))
  (:documentation "An error related to a += syntax."))

(define-condition cmdline-junk-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The piece of junk appearing on the command-line."
    :initarg :junk
    :reader junk))
  (:report (lambda (error stream)
	     (format stream "Junk on the command-line: ~S." (junk error))))
  (:documentation "An error related to a command-line piece of junk."))

(defun restartable-cmdline-junk-error (junk)
  (restart-case (error 'cmdline-junk-error :junk junk)
    (discard ()
      :report "Discard junk."
      nil)))

(define-condition unrecognized-short-call-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The unrecognized short call on the command-line."
    :initarg :short-call
    :reader short-call))
  (:report (lambda (error stream)
	     (format stream "Unrecognized short call: ~S." (short-call error))))
  (:documentation "An error related to an unrecognized short call."))

(define-condition unrecognized-plus-call-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The unrecognized plus call on the command-line."
    :initarg :plus-call
    :reader plus-call))
  (:report (lambda (error stream)
	     (format stream "Unrecognized plus call: ~S." (plus-call error))))
  (:documentation "An error related to an unrecognized plus call."))

(define-condition unknown-cmdline-option-error (cmdline-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The option's name as it appears on the command-line."
    :initarg :name
    :reader name)
   (argument :documentation "The option's command-line argument."
	     :initarg :argument
	     :reader argument))
  (:report (lambda (error stream)
	     (format stream "Unknown command-line option ~
			    ~S~@[ with argument ~S~]."
	       (name error)
	       (argument error))))
  (:documentation "An error related to an unknown command-line option."))


;; ============================================================================
;; The Context Class
;; ============================================================================

(defclass context ()
  ((synopsis :documentation "The program synopsis."
	     :type synopsis
	     :initarg :synopsis
	     :reader synopsis)
   (progname :documentation "The program name, as it appears on the command-line."
	     :type string
	     :reader progname)
   (cmdline-options :documentation "The options from the command-line."
	  :type list
	  :accessor cmdline-options)
   (remainder :documentation "The non-Clon part of the command-line."
	      :type list
	      :reader remainder)
   (search-path :documentation "The search path for Clon files."
		:reader search-path)
   (theme :documentation "The theme filename."
	  :reader theme)
   (highlight :documentation "Whether to highlight Clon's output."
	      :reader highlight)
   (error-handler :documentation ~"The behavior to adopt on errors "
			       ~"at command-line parsing time."
		  :type symbol
		  :initarg :error-handler
		  :initform :quit
		  :reader error-handler)
   (getopt-error-handler :documentation ~"The default behavior to adopt on errors "
				     ~"in the getopt family of functions."
			:type symbol
			:initarg :getopt-error-handler
			:initform :quit
			:reader getopt-error-handler))
  (:default-initargs
      ;; #### PORTME.
      :cmdline sb-ext:*posix-argv*)
  (:documentation "The CONTEXT class.
This class represents the associatiion of a synopsis and a set of command-line
options based on it."))

(defmethod initialize-instance :before ((context context) &key synopsis)
  "Ensure that SYNOPSIS is sealed."
  (unless (sealedp synopsis)
    (error "Initializing context ~A: synopsis ~A not sealed." context synopsis)))

(defun read-long-name ()
  "Read an option's long name from standard input."
  (format t "Please type in the correct option's long name:~%")
  (let (line)
    (loop (setq line (read-line))
	(if (position #\= line)
	    (format t "Option names can't contain equal signs. Try again:~%")
	    (return (list line))))))

(defun read-call (&optional plus)
  "Read an option's call or pack from standard input.
If PLUS, read a plus call or pack. Otherwise, read a short call or minus pack."
  (format t "Please type in the correct ~
	    ~:[short call or minus~;plus call or~] pack:~%"
    plus)
  (list (read-line)))

(defmethod initialize-instance :after ((context context) &key cmdline)
  "Parse CMDLINE."
  (setf (slot-value context 'progname) (pop cmdline))
  ;; Step one: parse the command-line ========================================
  (let ((cmdline-options (list))
	(remainder (list)))
    (macrolet ((push-cmdline-option (place &rest body)
		 "Push a new CMDLINE-OPTION created with BODY onto PLACE."
		 `(push (make-cmdline-option ,@body) ,place))
	       (push-retrieved-option
		   (place func option &optional cmdline-value cmdline name-form)
		   "Retrieve OPTION from a FUNC call and push it onto PLACE.
- FUNC must be either :long, :short or :plus,
- CMDLINE-VALUE is a potentially already parsed option argument,
- CMDILNE is where to find a potentially required argument,
- NAME-FORM is how to compute the :name slot of the CMDLINE-OPTION structure.
  If not given, the option's long or short name will be used as appropriate."
		   (let* ((value (gensym "value"))
			  (vars (list value))
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
			:value ,value))))
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
      (handler-bind ((cmdline-error
		      (lambda (error)
			(ecase (error-handler context)
			  (:quit
			   (let (*print-escape*)
			     (print-object error t))
			   (terpri)
			   (quit 1))
			  (:none)))))
	(do ((arg (pop cmdline) (pop cmdline)))
	    ((null arg))
	  (cond ((string= arg "--")
		 ;; The Clon separator.
		 (setq remainder cmdline)
		 (setq cmdline nil))
		((beginning-of-string-p "--" arg)
		 ;; A long call.
		 (let* ((value-start (position #\= arg :start 2))
			(cmdline-name (subseq arg 2 value-start))
			(cmdline-value (when value-start
					 (subseq arg (1+ value-start))))
			option-name option)
		   (tagbody find-option
		      (setq option-name cmdline-name)
		      (setq option
			    (search-option context :long-name cmdline-name))
		      (unless option
			(multiple-value-setq (option option-name)
			  (search-option context :partial-name cmdline-name)))
		      (if option
			  (push-retrieved-option cmdline-options :long option
						 cmdline-value cmdline
						 option-name)
			  (restart-case (error 'unknown-cmdline-option-error
					       :name cmdline-name
					       :argument cmdline-value)
			    (discard ()
			      :report "Discard unknown option."
			      nil)
			    (fix-option-name (new-cmdline-name)
			      :report "Fix the option's long name."
			      :interactive read-long-name
			      (setq cmdline-name new-cmdline-name)
			      (go find-option)))))))
		;; A short call, or a minus pack.
		((beginning-of-string-p "-" arg)
		 (tagbody figure-this-short-call
		    (let* ((value-start (position #\= arg :start 2))
			   (cmdline-name (subseq arg 1 value-start))
			   (cmdline-value (when value-start
					    (subseq arg (1+ value-start))))
			   option)
		      (when cmdline-value
			(restart-case (error 'invalid--=-syntax :item arg)
			  (discard-argument ()
			    :report "Discard the argument."
			    (setq cmdline-value nil))
			  (stick-argument ()
			    :report "Stick argument to option name."
			    (setq cmdline-name (concatenate 'string
						 cmdline-name cmdline-value))
			    (setq cmdline-value nil))
			  (separate-argument ()
			     :report "Separate option from its argument."
			     (push cmdline-value cmdline)
			     (setq cmdline-value nil))))
		      (setq option
			    (search-option context :short-name cmdline-name))
		      (unless option
			(multiple-value-setq (option cmdline-value)
			  (search-sticky-option context cmdline-name)))
		      (cond (option
			     (push-retrieved-option cmdline-options :short
						    option cmdline-value
						    cmdline))
			    ((potential-pack-p cmdline-name context)
			     ;; #### NOTE: When parsing a minus pack, only the
			     ;; last option gets a cmdline argument because
			     ;; only the last one is allowed to retrieve an
			     ;; argument from there.
			     (do-pack (option
				       (subseq cmdline-name 0
					       (1- (length cmdline-name)))
				       context)
			       (push-retrieved-option cmdline-options :short
						      option))
			     (let* ((name (subseq cmdline-name
						  (1- (length cmdline-name))))
				    (option (search-option context
							   :short-name name)))
			       (assert option)
			       (push-retrieved-option
				cmdline-options :short option nil cmdline)))
			    (t
			     (restart-case
				 (error 'unrecognized-short-call-error
					:short-call cmdline-name)
			       (discard ()
				 :report "Discard this short call."
				 nil)
			       (fix-short-call (new-cmdline-name)
				 :report "Fix this short call."
				 :interactive (lambda () (read-call))
				 (setq arg (concatenate 'string
					     "-" new-cmdline-name))
				 (go figure-this-short-call))))))))
		;; A plus call or a plus pack.
		((beginning-of-string-p "+" arg)
		 (block processing-+-call
		   (tagbody figure-this-+-call
		      (let* ((value-start (position #\= arg :start 2))
			     (cmdline-name (subseq arg 1 value-start))
			     (cmdline-value (when value-start
					      (subseq arg (1+ value-start))))
			     option)
			(when cmdline-value
			  (restart-case (error 'invalid-+=-syntax :item arg)
			    (discard-argument ()
			      :report "Discard the argument."
			      (setq cmdline-value nil))
			    (convert-to-short-and-stick ()
			      :report "Convert to short call and stick argument."
			      (push (concatenate 'string
				      "-" cmdline-name cmdline-value)
				    cmdline)
			      (return-from processing-+-call))
			    (convert-to-short-and-split ()
			      :report "Convert to short call and split argument."
			      (push cmdline-value cmdline)
			      (push (concatenate 'string "-" cmdline-name)
				    cmdline)
			      (return-from processing-+-call))))
			;; #### NOTE: in theory, we could allow partial
			;; matches on short names when they're used with the
			;; +-syntax, because there's no sticky argument or
			;; whatever. But we don't. That's all. Short names are
			;; not meant to be long (otherwise, that would be long
			;; names right?), so they're not meant to be
			;; abbreviated.
			(setq option
			      (search-option context :short-name cmdline-name))
			(cond (option
			       (push-retrieved-option cmdline-options :plus
						      option))
			      ((potential-pack-p cmdline-name context)
			       (do-pack (option cmdline-name context)
				 (push-retrieved-option cmdline-options :plus
							option)))
			      (t
			       (restart-case
				   (error 'unrecognized-plus-call-error
					  :plus-call cmdline-name)
				 (discard ()
				   :report "Discard this plus call."
				   nil)
				 (fix-plus-call (new-cmdline-name)
				   :report "Fix this plus call."
				   :interactive (lambda () (read-call :plus))
				   (setq arg (concatenate 'string
					       "+" new-cmdline-name))
				   (go figure-this-+-call)))))))))
		(t
		 ;; Not an option call. If there's no more option on the
		 ;; cmdline, consider this as an implicit remainder. However,
		 ;; contrary to the case of an explicit one (separated from
		 ;; the rest of the cmdline by --), trigger an error if a
		 ;; remainder is not expected. If there's still another option
		 ;; somewhere, then this is real junk.
		 (cond ((notany #'option-call-p cmdline)
			(cond ((null (postfix context))
			       (setq arg (cons arg cmdline))
			       (setq cmdline nil)
			       ;; Note that here, the whole remainder of the
			       ;; cmdline might be discraded at once.
			       (restartable-cmdline-junk-error arg))
			      (t
			       (setq remainder (cons arg cmdline))
			       (setq cmdline nil))))
		       (t
			(restartable-cmdline-junk-error arg)))))))
      (setf (cmdline-options context) (nreverse cmdline-options))
      (setf (slot-value context 'remainder) remainder)))
  ;; Step two: Treat internal options ========================================
  (when (getopt context :long-name "clon-banner")
    (format t "~A is powered by the Clon library, version ~A,
written by Didier Verna <didier@lrde.epita.fr>.

Copyright (C) 2008 Didier Verna.
Clon is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.~%"
      (pathname-name (progname context))
      (version :long))
    (quit 0))
  (let ((version-format (getopt context :long-name "clon-version")))
    (when version-format
      (format t "~A~%" (version version-format))
      (quit 0)))
  (setf (slot-value context 'search-path)
	(getopt context :long-name "clon-search-path"))
  (setf (slot-value context 'theme)
	(getopt context :long-name "clon-theme"))
  (setf (slot-value context 'highlight)
	(getopt context :long-name "clon-highlight")))

(defun make-context
    (&rest keys &key synopsis error-handler getopt-error-handler cmdline)
  "Make a new context.
- SYNOPSIS is the program synopsis to use in that context.
- ERROR-HANDLER is the behavior to adopt on errors at command-line parsing time.
  It can be one of:
  * :quit, meaning print the error and abort execution,
  * :none, meaning let the debugger handle the situation.
- GETOPT-ERROR-HANDLER is the default behavior to adopt on command-line errors
  in the GETOPT family of functions (note that this behavior can be overridden
in the functions themselves). It can be one of:
  * :quit, meaning print the error and abort execution,
  * :none, meaning let the debugger handle the situation.
- CMDLINE is the argument list (strings) to process.
  It defaults to a POSIX conformant argv."
  (declare (ignore synopsis error-handler getopt-error-handler cmdline))
  (apply #'make-instance 'context keys))


(defmethod postfix ((context context))
  "Return the postfix of CONTEXT's synopsis."
  (postfix (synopsis context)))


;; -----------------------
;; Potential pack protocol
;; -----------------------

(defmethod potential-pack-p (pack (context context))
  "Return t if PACK (a string) is a potential pack in CONTEXT."
  (potential-pack-p pack (synopsis context)))



;; ===========================================================================
;; The Option Search Protocol
;; ===========================================================================

(defun search-option-by-name (context &rest keys &key short-name long-name)
  "Search for option with either SHORT-NAME or LONG-NAME in SYNOPSIS.
When such an option exists, return two values:
- the option itself,
- the name that matched."
  (declare (ignore short-name long-name))
  (do-options (option (synopsis context))
    (let ((name (apply #'match-option option keys)))
      (when name
	(return-from search-option-by-name (values option name))))))

(defun search-option-by-abbreviation (context partial-name)
  "Search for option abbreviated with PARTIAL-NAME in SYNOPSIS.
When such an option exists, return two values:
- the option itself,
- the completed name."
  (let ((shortest-distance most-positive-fixnum)
	closest-option)
    (do-options (option (synopsis context))
      (let ((distance (option-abbreviation-distance option partial-name)))
	(when (< distance shortest-distance)
	  (setq shortest-distance distance)
	  (setq closest-option option))))
    (when closest-option
      (values closest-option
	      (complete-string partial-name (long-name closest-option))))))

(defun search-option (context &rest keys &key short-name long-name partial-name)
  "Search for an option in CONTEXT.
The search is done with SHORT-NAME, LONG-NAME, or PARTIAL-NAME.
In case of a PARTIAL-NAME search, look for an option the long name of which
begins with it.
In case of multiple matches by PARTIAL-NAME, the longest match is selected.
When such an option exists, return wo values:
- the option itself,
- the name used to find the option, possibly completed if partial."
  (econd ((or short-name long-name)
	  (apply #'search-option-by-name context keys))
	 (partial-name
	  (search-option-by-abbreviation context partial-name))))

(defun search-sticky-option (context namearg)
  "Search for a sticky option in CONTEXT, matching NAMEARG.
NAMEARG is the concatenation of the option's short name and its argument.
In case of multiple matches, the option with the longest name is selected.
When such an option exists, return two values:
- the option itself,
- the argument part of NAMEARG."
  (let ((longest-distance 0)
	closest-option)
    (do-options (option (synopsis context))
      (let ((distance (option-sticky-distance option namearg)))
	(when (> distance longest-distance)
	  (setq longest-distance distance)
	  (setq closest-option option))))
    (when closest-option
      (values closest-option (subseq namearg longest-distance)))))



;; ============================================================================
;; The Option Retrieval Protocol
;; ============================================================================

(defun getopt (context &rest keys
		       &key short-name long-name option
			    (error-handler (getopt-error-handler context)))
  "Get an option's value in CONTEXT.
The option can be specified either by SHORT-NAME, LONG-NAME, or directly via
an OPTION object.
ERROR-HANDLER is the behavior to adopt on errors. Its default value depends on
the CONTEXT. See `make-context' for a list of possible values. Note that
command-line errors are treated at context-creation time, so the only errors
that can occur here are coming from the environment.
This function returns two values:
- the retrieved value,
- the value's source."
  (unless option
    (setq option
	  (apply #'search-option context (remove-keys keys :error-handler))))
  (unless option
    (error "Getting option ~S from synopsis ~A in context ~A: unknown option."
	   (or short-name long-name)
	   (synopsis context)
	   context))
  ;; Try the command-line:
  (let ((cmdline-options (list)))
    (do ((cmdline-option
	  (pop (cmdline-options context))
	  (pop (cmdline-options context))))
	((null cmdline-option))
      (cond ((eq (cmdline-option-option cmdline-option) option)
	     (setf (cmdline-options context)
		   ;; Actually, I *do* have a use for nreconc ;-)
		   (nreconc cmdline-options (cmdline-options context)))
	     (return-from getopt
	       (values (cmdline-option-value cmdline-option)
		       (list :cmdline (cmdline-option-name cmdline-option)))))
	    (t
	     (push cmdline-option cmdline-options))))
    (setf (cmdline-options context) (nreverse cmdline-options)))
  ;; Try an environment variable:
  (handler-bind ((environment-error
		  (lambda (error)
		    (ecase error-handler
		      (:quit
		       (let (*print-escape*) (print-object error t))
		       (terpri)
		       ;; #### PORTME.
		       (sb-ext:quit :unix-status 1))
		      (:none)))))
    (let* ((env-var (env-var option))
	   ;; #### PORTME.
	   ;; #### Note: (sb-posix:getenv nil) -> nil, so using let* is ok.
	   (env-val (sb-posix:getenv env-var)))
      (when env-val
	(return-from getopt
	  (values (retrieve-from-environment option env-val)
		  (list :environement env-var))))))
  ;; Try a default value:
  (when (and (typep option 'valued-option)
	     (slot-boundp option 'default-value))
    (values (default-value option) (list :default-value))))

(defun getopt-cmdline (context)
  "Get the next cmdline option in CONTEXT.
This function returns three values:
- the option object,
- the option's name used on the command-line,
- the retrieved value."
  (let ((cmdline-option (pop (cmdline-options context))))
    (when cmdline-option
      (values (cmdline-option-option cmdline-option)
	      (cmdline-option-name cmdline-option)
	      (cmdline-option-value cmdline-option)))))

(defmacro multiple-value-getopt-cmdline ((option name value) context &body body)
  "Evaluate BODY on the next command-line option in CONTEXT.
OPTION, NAME and VALUE are bound to the option's object, name used on the
command-line) and retrieved value."
  `(multiple-value-bind (,option ,name ,value) (getopt-cmdline ,context)
    ,@body))

(defmacro do-cmdline-options ((option name value) context &body body)
  "Evaluate BODY over all command-line options in CONTEXT.
OPTION, NAME and VALUE are bound to each option's object, name used on the
command-line) and retrieved value."
  `(do () ((null (cmdline-options ,context)))
    (multiple-value-getopt-cmdline (,option ,name ,value) ,context
      ,@body)))


;;; context.lisp ends here
