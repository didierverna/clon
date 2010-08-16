;;; environ.lisp --- Environment management

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Oct 22 10:12:37 2008
;; Last Revision: Sat Jun 12 18:27:49 2010

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
;; Environmental Error Management
;; ==========================================================================

;; #### NOTE: currently, there is only one environment error: an invalid value
;; for an environment variable associated with an option. This means that the
;; ENV-VAR slot below is redundant, because the environment variable can be
;; accessed through the option object. However, the design is cleaner this way
;; (it is coherent with the command-line one), and maybe if one day I have to
;; extend it, I'll be happy I got it right in the first place.
;; #### NOTE: as a matter of fact, I just thought of something: what about
;; supporting a /list/ of associated environment variables? This would
;; perfectly justify the comment above.
(define-condition environment-error (error)
  ((env-var :documentation "The concerned environment variable."
	    :type string
	    :initarg :env-var
	    :reader env-var))
  (:documentation "An error related to an environment variable."))

(define-condition environmental-option-error (environment-error option-error)
  ()
  (:documentation "An error related to an option's environment variable."))

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



;; ==========================================================================
;; The Environement Retrieval Protocol
;; ==========================================================================

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

;; #### FIXME: we should split this into flag.lisp and valued.lisp but we
;; can't because this fiel is loaded only after the options files.


;; #### WARNING: given the idea of supporting a list of env vars, I would need
;; to modify this function in order to pass the env-var itself. This protocol
;; would become even more similar to the command-line one. Yummy...
(defgeneric retrieve-from-environment (option env-val)
  (:documentation "Retrieve OPTION's value from the environment.
ENV-VAL is the value stored in the associated environment variable.")
  (:method :before (option env-val)
     "Assert that ENV-VAL is not null."
     ;; That's because getopt is not supposed to call this function unless
     ;; there is actually something to retrieve.
     (assert env-val))
  ;; Method for flags:
  (:method ((flag flag) env-val)
    (declare (ignore env-val))
    ;; #### NOTE: there's no way of providing an env var /without/ a value
    ;; (the value is at least the empty string). Consequently, we decide that
    ;; the presence of the env var, regardless of its value, stands for the
    ;; presence of the flag.
    t)
  ;; Method for all valued options:
  (:method ((option valued-option) env-val)
    (restartable-environment-convert option env-val)))


;;; environ.lisp ends here
