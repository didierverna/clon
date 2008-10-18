;;; valued.lisp --- Valued options for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:25:03 2008
;; Last Revision: Tue Oct  7 21:27:19 2008

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
;; Utilities
;; ============================================================================

;; #### TODO: Yuck. There are places in this file, like right here, where some
;; notion of the command-line syntax is needed. This is not very nice because
;; the command-line syntax should ideally be known only to context.lisp.
;; However, since the retrieval process changes according to the option
;; classes, it is still reasonable to have it here.

;; A better design would be a dialog between the context level and the option
;; one. Like:
;; - CONTEXT: retreive-from-long-call option arg-from-=-syntax [= nil],
;; - OPTION: Hey, I need an argument, but you didn't give me one,
;; - CONTEXT: Okay, lemme see what's next on the command-line...
;;            ... Sorry, nothing for you. BARF.

(defun option-call-p (str)
  "Return true if STR looks like an option call."
  (or (eq (elt str 0) #\-)
      (eq (elt str 0) #\+)))

(defun argument-popable-p (cmdline)
  "Return true if the first CMDLINE item is an argument."
  (and (car cmdline)
       (not (option-call-p (car cmdline)))))

(defmacro maybe-pop-argument (cmdline option cmdline-argument)
  "Pop OPTION's argument from CMDLINE if needed.
If so, store it into CMDLINE-ARGUMENT."
  ;; At the time this macro is called, CMDLINE-ARGUMENT may already contain
  ;; something, provided by either a sticky argument from a short call, or an
  ;; =-syntax from a long call. Remember that these are the only 2 ways to
  ;; provide optional arguments, so the need to pop something occurs only
  ;; when an argument is mandatory, and it is still missing.
  `(when (and (null ,cmdline-argument)
	      (argument-required-p ,option)
	      (argument-popable-p ,cmdline))
     (setq ,cmdline-argument (pop ,cmdline))))


;; ============================================================================
;; The Valued Option Class
;; ============================================================================

(defabstract valued-option (option)
  ((argument-name :documentation "The option's argument display name."
		  :initarg :argument-name
		  :initform "ARG"
		  :reader argument-name)
   (argument-required-p :documentation "Whether the option's argument is required."
			;; Initialization :after wards by :argument-type
			:reader argument-required-p)
   (fallback-value :documentation "The option's fallback value."
		   :initarg :fallback-value
		   :reader fallback-value)
   (default-value :documentation "The option's default value."
		 :initarg :default-value
		 :reader default-value))
  (:default-initargs
    :argument-type :required)
  (:documentation "The VALUED-OPTION class.
This is the base class for options accepting arguments."))

(defclass valued-option-class (standard-class)
  ()
  (:documentation "The VALUED-OPTION-CLASS class.
This is the meta-class for all valued options, that is, for all
subclasses of the VALUED-OPTION class."))

(defvar *valued-option-names* nil
  "The list of known valued option names.")

(defmethod initialize-instance :after
    ((class valued-option-class) &key direct-superclasses direct-slots)
  "Register CLASS as a new valued option class."
  (declare (ignore direct-superclasses direct-slots))
  (pushnew (symbol-name (class-name class)) *valued-option-names*))

(defmacro defoption (class superclasses slots &rest options)
  "Wrapper around defclass for defining a new Clon valued option class."
  (when (assoc :metaclass options)
    (error "Defining valued option class ~S: explicit meta-class option." class))
  `(defclass ,class ,(cons 'valued-option superclasses)
    ,slots
    ,@options
    (:metaclass valued-option-class)))

(defmethod initialize-instance :before
    ((option valued-option) &key argument-type
			       (fallback-value nil fallback-value-supplied-p)
			       (default-value nil default-value-supplied-p))
  "Check validity of the value-related initargs."
  (unless (member argument-type '(:required :mandatory :optional))
    (error "Option ~A: invalid argument type ~S." option argument-type))
  (when (and (not (eq argument-type :optional))
	     fallback-value-supplied-p)
    (warn "Option ~A: fallback value supplied for required argument." option))
  (when (and (eq argument-type :optional)
	     (not fallback-value-supplied-p)
	     (not default-value-supplied-p))
    (error "Option ~A: fallback or default value required for optional argument."
	   option))
  ;; Here, we catch and convert a potential invalid-value error into a simple
  ;; error because this check is intended for the Clon user, as opposed to the
  ;; Clon end-user. In other words, a potential error here is in the program
  ;; itself; not in the usage of the program.
  (when fallback-value-supplied-p
    (handler-case (check-value option fallback-value)
      (invalid-value ()
	(error "Option ~A: invalid fallback value ~S." option fallback-value))))
  (when default-value-supplied-p
    (handler-case (check-value option default-value)
      (invalid-value ()
	(error "Option ~A: invalid default value ~S." option default-value)))))

(defmethod initialize-instance :after
    ((option valued-option) &key argument-type)
  "Compute uninitialized OPTION slots with indirect initargs.
This currently involves the conversion of the ARGUMENT-TYPE key to the
ARGUMENT-REQUIRED-P slot."
  (ecase argument-type
    ((:required :mandatory)
     (setf (slot-value option 'argument-required-p) t))
    (:optional
     (setf (slot-value option 'argument-required-p) nil))))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod option-sticky-distance ((option valued-option) namearg)
  "Try to match OPTION's short name with a sticky argument against NAMEARG.
If OPTION matches, return its short name's length; otherwise 0."
  (with-slots (short-name) option
    (cond ((and short-name (beginning-of-string-p short-name namearg))
	   ;; This case should not happen because we always look for a
	   ;; complete match before looking for a sticky match.
	   (assert (not (string= namearg short-name)))
	   (length short-name))
	  (t
	   0))))


;; -------------------
;; Char packs protocol
;; -------------------

;; Options with a one-character short name and requiring an argument may
;; appear as the last option in a minus pack. However, we don't make them
;; appear in the usage string. This is why this function filters out options
;; with mandatory argument.
(defmethod minus-pack-char ((option valued-option) &optional as-string)
  "Return OPTION's minus pack character if OPTION's argument is optional."
  (unless (argument-required-p option)
    (potential-pack-char option as-string)))


;; ===========================================================================
;; The Conversion Protocol
;; ===========================================================================

(define-condition invalid-value (option-error)
  ((value :documentation "The invalid value."
	  :initarg :value
	  :reader value)
   (comment :documentation "An additional comment about the error."
	    :type string
	    :initarg :comment
	    :reader comment))
  (:report (lambda (error stream)
	     (format stream "Option ~A: invalid value ~S.~@[~%~A~]"
	       (option error) (value error) (comment error))))
  (:documentation "An invalid value error."))

(defun read-value ()
  "Read an option value from standard input."
  (format t "Please type in the new value:~%")
  (list (read)))

(defgeneric check-value (valued-option value)
  (:documentation "Check that VALUE is valid for VALUED-OPTION.
If VALUE is valid, return it. Otherwise, raise an invalid-value error."))

(defun restartable-check-value (valued-option value)
  "Restartably check that VALUE is valid for VALUED-OPTION.
The only restart available, use-value, offers to try a different value from
the one that was provided."
  (restart-case (check-value valued-option value)
    (use-value (value)
      :report "Use another value instead."
      :interactive read-value
      (restartable-check-value valued-option value))))

(define-condition invalid-argument (option-error)
  ((argument :documentation "The invalid argument."
	     :type string
	     :initarg :argument
	     :reader argument)
   (comment :documentation "An additional comment about the error."
	    :type string
	    :initarg :comment
	    :reader comment))
  (:report (lambda (error stream)
	     (format stream "Option ~A: invalid argument ~S.~@[~%~A~]"
	       (option error) (argument error) (comment error))))
  (:documentation "An invalid argument error."))

(defun read-argument ()
  "Read an option argument from standard input."
  (format t "Please type in the new argument:~%")
  (list (read-line)))

(defgeneric convert (valued-option argument)
  (:documentation "Convert ARGUMENT to VALUED-OPTION's value.
If ARGUMENT is invalid, raise an invalid-argument error."))

;; #### NOTE: the restarts provided here are actually not used because
;; conversion errors are caught by a handler-case in the retrieval routines,
;; which provide higher-level errors and restarts. I leave them here however,
;; because they might be useful for debugging.
(defun restartable-convert (valued-option argument)
  "Restartably convert ARGUMENT to VALUED-OPTION's value.
Available restarts are:
- use-default-value: return OPTION's default value,
- use-value: return another (already converted) value,
- use-argument: return the conversion of another argument."
  (restart-case (convert valued-option argument)
    (use-default-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (slot-boundp valued-option 'default-value))
      :report (lambda (stream)
		(format stream "Use option's default value (~S) instead."
		  (default-value valued-option)))
      (default-value valued-option))
    (use-value (value)
      :report "Use another (already converted) value."
      :interactive read-value
      (restartable-check-value valued-option value))
    (use-argument (argument)
      :report "Use another (to be converted) argument."
      :interactive read-argument
      (restartable-convert valued-option argument))))


;; ------------------
;; Retrieval protocol
;; ------------------

(define-condition invalid-cmdline-argument (cmdline-option-error invalid-argument)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': invalid argument ~S.~@[~%~A~]"
	       (name error) (argument error) (comment error))))
  (:documentation "An invalid command-line argument error."))

(defun cmdline-convert (valued-option cmdline-name cmdline-argument)
  "Convert CMDLINE-ARGUMENT to VALUED-OPTION's value.
This function is used when the conversion comes from a command-line usage of
VALUED-OPTION, called by CMDLINE-NAME, and intercepts invalid-argument errors
to raise the higher level invalid-cmdline-argument error instead."
  (handler-case (restartable-convert valued-option cmdline-argument)
    (invalid-argument (error)
      (error 'invalid-cmdline-argument
	     :option valued-option
	     :name cmdline-name
	     :argument cmdline-argument
	     :comment (comment error)))))

(define-condition missing-cmdline-argument (cmdline-option-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': missing argument." (name error))))
  (:documentation "A missing command-line argument error."))

(defun restartable-cmdline-convert (valued-option cmdline-name cmdline-argument)
  "Restartably convert CMDLINE-ARGUMENT to VALUED-OPTION's value.
This function is used when the conversion comes from a command-line usage of
VALUED-OPTION, called by CMDLINE-NAME.

As well as conversion errors, this function might raise a
missing-cmdline-argument error if CMDLINE-ARGUMENT is nil and an argument is
required.

Available restarts are (depending on the context):
- use-fallback-value: return FALLBACK-VALUE,
- use-default-value: return VALUED-OPTION's default value,
- use-value: return another (already converted) value,
- use-argument: return the conversion of another argument."
  (restart-case
      (cond ((argument-required-p valued-option)
	     (if cmdline-argument
		 (cmdline-convert valued-option cmdline-name cmdline-argument)
		 (error 'missing-cmdline-argument
			:option valued-option :name cmdline-name)))
	    (cmdline-argument
	     (cmdline-convert valued-option cmdline-name cmdline-argument))
	    (t
	     (if (slot-boundp valued-option 'fallback-value)
		 (fallback-value valued-option)
		 (default-value valued-option))))
    (use-fallback-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (and (not (argument-required-p valued-option))
		   (slot-boundp valued-option 'fallback-value)))
      :report (lambda (stream)
		(format stream "Use fallback value (~S)."
		  (fallback-value valued-option)))
      (fallback-value valued-option))
    (use-default-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (slot-boundp valued-option 'default-value))
      :report (lambda (stream)
		(format stream "Use option's default value (~S)."
		  (default-value valued-option)))
      (default-value valued-option))
    (use-value (value)
      :report "Use an already converted value."
      :interactive read-value
      (restartable-check-value valued-option value))
    (use-argument (cmdline-argument)
      :report "Use the conversion of an argument."
      :interactive read-argument
      (restartable-cmdline-convert
       valued-option cmdline-name cmdline-argument))))

(defmethod retrieve-from-long-call
    ((option valued-option) cmdline-name &optional cmdline-argument cmdline)
  "Retrieve OPTION's value from a long call."
  (maybe-pop-argument cmdline option cmdline-argument)
  (values (restartable-cmdline-convert option cmdline-name cmdline-argument)
	  cmdline))

(defmethod retrieve-from-short-call
    ((option valued-option) &optional cmdline-argument cmdline)
  "Retrieve OPTION's value from a short call."
  (maybe-pop-argument cmdline option cmdline-argument)
  (values (restartable-cmdline-convert option (short-name option) cmdline-argument)
	  cmdline))

;; This method applies to all valued options but the switches.
(defmethod retrieve-from-plus-call ((option valued-option))
  "Throw an invalid-+-syntax error."
  (restartable-invalid-+-syntax-error (option)
    (retrieve-from-short-call option)))

(define-condition invalid-environment-value
    (environmental-option-error invalid-argument)
  ((argument ;; inherited from the INVALID-ARGUMENT condition
    :documentation "The invalid environment variable value."
    :initarg :env-val
    :reader env-val))
  (:report
   (lambda (error stream)
     (format stream "Environment variable ~A (for option ~S): ~
		    invalid value ~S.~@[~%~A~]"
       (env-var error)
       (or (long-name (option error)) (short-name (option error)))
       (env-val error)
       (comment error))))
  (:documentation "An invalid environment variable's value error."))

;; #### WARNING: given the idea of supporting a list of env vars, I would need
;; to modify this function in order to pass the env-var itself. This protocol
;; would become even more similar to the command-line one. Yummy...
(defun environment-convert (valued-option env-val)
  "Convert ENV-VAL to VALUED-OPTION's value.
This function is used when the conversion comes from an environment variable
associated with VALUED-OPTION, and intercepts invalid-argument errors
to raise the higher level invalid-environment-value error instead."
  (handler-case (restartable-convert valued-option env-val)
    (invalid-argument (error)
      (error 'invalid-environment-value
	     :option valued-option
	     :env-var (env-var valued-option)
	     :env-val env-val
	     :comment (comment error)))))

(defun read-env-val (env-var)
  "Read ENV-VAR's new value from standard input."
  (format t "Please type in a new value for the ~A environment variable:~%"
    env-var)
  (list (read-line)))

;; #### WARNING: given the idea of supporting a list of env vars, I would need
;; to modify this function in order to pass the env-var itself. This protocol
;; would become even more similar to the command-line one. Yummy...
(defun restartable-environment-convert (valued-option env-val)
  "Restartably convert ENV-VAL to VALUED-OPTION's value.
This function is used when the conversion comes from an environment variable
associated with VALUED-OPTION.

Available restarts are:
- use-default-value: return VALUED-OPTION's default value,
- use-value: return another (already converted) value,
- use-argument: return the conversion of another argument,
- modify-env: modify the environment variable's value."
  (restart-case (environment-convert valued-option env-val)
    (use-default-value ()
      :test (lambda (error)
	      (declare (ignore error))
	      (slot-boundp valued-option 'default-value))
      :report (lambda (stream)
		(format stream "Use option's default value (~S)."
		  (default-value valued-option)))
      (default-value valued-option))
    (use-value (value)
      :report "Use an already converted value."
      :interactive read-value
      (restartable-check-value valued-option value))
    (use-argument (argument)
      :report "Use the conversion of an argument."
      :interactive read-argument
      (restartable-environment-convert valued-option argument))
    (modify-environment (env-val)
      :report "Modify the environment variable's value."
      :interactive (lambda () (read-env-val (env-var valued-option)))
      ;; #### PORTME.
      (sb-posix:putenv (concatenate 'string (env-var valued-option) "=" env-val))
      (restartable-environment-convert valued-option env-val))))

(defmethod retrieve-from-environment ((option valued-option) env-val)
  "Retrieve OPTION's value from the environment."
  (restartable-environment-convert option env-val))


;;; valued.lisp ends here
