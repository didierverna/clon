;;; cmdline.lisp --- Command-line management

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Oct 22 09:20:48 2008
;; Last Revision: Sat Jun 12 18:27:27 2010

;; This file is part of Clon.

;; Clon is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License version 3,
;; as published by the Free Software Foundation.

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

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; ==========================================================================
;; Utilities
;; ==========================================================================

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
If so, store it in CMDLINE-ARGUMENT."
  ;; At the time this macro is called, CMDLINE-ARGUMENT may already contain
  ;; something, provided by either a sticky argument from a short call, or an
  ;; =-syntax from a long call. Remember that these are the only 2 ways to
  ;; provide optional arguments, so the need to pop something occurs only
  ;; when an argument is mandatory, and it is still missing.
  `(when (and (null ,cmdline-argument)
	      (argument-required-p ,option)
	      (argument-popable-p ,cmdline))
     (setq ,cmdline-argument (pop ,cmdline))))



;; ==========================================================================
;; Command-line error management (regarding known options)
;; ==========================================================================

(define-condition cmdline-error (error)
  ((item :documentation "The concerned command-line item."
	 :type string
	 :initarg :item
	 :reader item))
  (:documentation "An error related to a command-line item."))

(define-condition cmdline-option-error (cmdline-error option-error)
  ((item ;; inherited from the CMDLINE-ERROR condition
    :documentation "The option's name as it appears on the command-line."
    :initarg :name
    :reader name))
  (:documentation "An error related to a command-line (known) option."))

(define-condition spurious-cmdline-argument (cmdline-option-error)
  ((argument :documentation "The spurious argument."
	     :type string
	     :initarg :argument
	     :reader argument))
  (:report (lambda (error stream)
	     (format stream "Option '~A': spurious argument ~S."
	       (name error) (argument error))))
  (:documentation "A spurious command-line argument error."))

;; #### NOTE: this macro is currently used only once: in the flags long call
;; retrieval process.
(defmacro restartable-spurious-cmdline-argument-error
    ((option name argument) &body body)
  "Restartably throw a spurious-cmdline-argument error.
The error relates to the command-line use of OPTION called by NAME with
ARGUMENT.
BODY constitutes the body of the only restart available, discard-argument, and
should act as if ARGUMENT had not been provided."
  `(restart-case (error 'spurious-cmdline-argument
		  :option ,option
		  :name ,name
		  :argument ,argument)
    (discard-argument ()
     :report "Discard spurious argument."
     ,@body)))

(define-condition invalid-+-syntax (cmdline-option-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': invalid +-syntax." (name error))))
  (:documentation "An invalid +-syntax error."))

(defmacro restartable-invalid-+-syntax-error ((option) &body body)
  "Restartably throw an invalid-+-syntax error.
The error relates to the command-line use of OPTION.
BODY constitutes the body of the only restart available,
use-short-call, and should act as if OPTION had been normally called by short
name."
  `(restart-case (error 'invalid-+-syntax
		  :option ,option
		  :name (short-name ,option))
    (use-short-call ()
     :report "Fake a normal call by short name."
     ,@body)))

(define-condition invalid-cmdline-argument (cmdline-option-error invalid-argument)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': invalid argument ~S.~@[~%~A~]"
	       (name error) (argument error) (comment error))))
  (:documentation "An invalid command-line argument error."))

(define-condition missing-cmdline-argument (cmdline-option-error)
  ()
  (:report (lambda (error stream)
	     (format stream "Option '~A': missing argument." (name error))))
  (:documentation "A missing command-line argument error."))



;; ==========================================================================
;; The Command-Line Retrieval Protocol
;; ==========================================================================

;; #### FIXME: we should split this into flag.lisp and valued.lisp but we
;; can't because this fiel is loaded only after the options files.

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

(defgeneric retrieve-from-long-call
    (option cmdline-name &optional cmdline-argument cmdline)
  (:documentation "Retrieve OPTION's value from a long call.
CMDLINE-NAME is the name used on the command-line.
CMDLINE-ARGUMENT is a potentially already parsed cmdline argument.
Otherwise, CMDLINE is where to find an argument.
This function returns two values:
- the retrieved value,
- the new command-line (possibly with the first item popped if the option
  requires an argument).")
  ;; Method for non-valued options (currently, only flags):
  (:method ((option option) cmdline-name  &optional cmdline-argument cmdline)
    ;; CMDLINE-ARGUMENT might be non-nil when a non-valued option was given a
    ;; spurious argument through an =-syntax.
    (if cmdline-argument
	(restartable-spurious-cmdline-argument-error
	    (option cmdline-name cmdline-argument)
	  (values t cmdline))
	(values t cmdline)))
  ;; Method for all valued options:
  (:method ((option valued-option) cmdline-name &optional cmdline-argument cmdline)
    (maybe-pop-argument cmdline option cmdline-argument)
    (values
     (restartable-cmdline-convert option cmdline-name cmdline-argument)
     cmdline)))

(defgeneric retrieve-from-short-call (option &optional cmdline-argument cmdline)
  (:documentation "Retrieve OPTION's value from a short call.
CMDLINE-ARGUMENT is a potentially already parsed cmdline argument.
Otherwise, CMDLINE is where to find an argument.
This function returns two values:
- the retrieved value,
- the new command-line (possibly with the first item popped if the option
  requires an argument).")
  ;; Method for non-valued options (currently, only flags):
  (:method ((option option) &optional cmdline-argument cmdline)
    ;; See comment about this assertion in OPTION-STICKY-DISTANCE.
    (assert (null cmdline-argument))
    (values t cmdline))
  ;; Method for valued options:
  (:method ((option valued-option) &optional cmdline-argument cmdline)
    (maybe-pop-argument cmdline option cmdline-argument)
    (values
     (restartable-cmdline-convert option (short-name option) cmdline-argument)
     cmdline)))

(defgeneric retrieve-from-plus-call (option)
  (:documentation "Retrieve OPTION's value from a plus call.")
  ;; Method for non-valued options (currently, only flags):
  (:method ((option option))
    (restartable-invalid-+-syntax-error (option)
      t))
  ;; Method for valued options:
  (:method ((option valued-option))
    (restartable-invalid-+-syntax-error (option)
      (retrieve-from-short-call option)))
  ;; Method for plus-callable options (currently, the switch hierarchy):
  (:method ((plus-callable plus-callable))
    nil))


;;; cmdline.lisp ends here
