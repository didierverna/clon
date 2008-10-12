;;; flag.lisp --- Flag options for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Oct  7 21:22:05 2008
;; Last Revision: Tue Oct  7 21:25:49 2008

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
;; The Flag Class
;; ============================================================================

;; A flag can appear in the following forms:

;; -f, --flag                           both names
;; -f                                   short name
;; --flag                               long name

(defclass flag (option)
  ()
  (:documentation "The FLAG class.
This class implements options that don't take any argument."))

(defun make-flag (&rest keys &key short-name long-name description env-var)
  "Make a new flag.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the flag's associated environment variable.
  It defaults to nil."
  (declare (ignore short-name long-name description env-var))
  (apply #'make-instance 'flag keys))

(defun make-internal-flag (long-name description &optional env-var)
  "Make a new internal (Clon-specific) flag.
- LONG-NAME is the flag's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the flag's description.
- ENV-VAR is the flag's associated environment variable, minus the 'CLON_'
  prefix. It default to nil."
  (make-instance 'flag
    :long-name long-name
    :description description
    :env-var env-var
    :internal t))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod option-sticky-distance ((flag flag) namearg)
  "Return 0 (flags don't take any argument, sticky or not)."
  ;; #### NOTE: there is something a bit shaky here: this function is called
  ;; during cmdline parsing (so this is really a lexico-syntactic analysis
  ;; stage), but we return 0 because of a semantic point concerning flags:
  ;; they don't take arguments. The consequence is that flags won't ever get a
  ;; cmdline-argument in retrieve-from-short-call, hence the assertion there.
  (declare (ignore namearg))
  0)


;; -------------------
;; Char packs protocol
;; -------------------

(defmethod minus-pack-char ((flag flag) &optional as-string)
  "Return FLAG's minus pack character, if any."
  ;; Since flags don't take any argument, being minus-packable is the same as
  ;; being potentially packable.
  (potential-pack-char flag as-string))


;; -------------------
;; Retrieval protocol
;; -------------------

(defmethod retrieve-from-long-call
    ((flag flag) cmdline-name  &optional cmdline-argument cmdline)
  "Retrieve FLAG's value from a long call."
  ;; CMDLINE-ARGUMENT might be non-nil when a flag was given a spurious
  ;; argument through an =-syntax.
  (if cmdline-argument
      (restartable-spurious-cmdline-argument-error
	  (flag cmdline-name cmdline-argument)
	(values t cmdline))
      (values t cmdline)))

(defmethod retrieve-from-short-call
    ((flag flag) &optional cmdline-argument cmdline)
  "Retrieve FLAG's value from a short call."
  ;; See comment about this assertion in option-sticky-distance.
  (assert (null cmdline-argument))
  (values t cmdline))

(defmethod retrieve-from-plus-call ((flag flag))
  "Throw an invalid-+-syntax error."
  (restartable-invalid-+-syntax-error (flag) t))

(defmethod retrieve-from-environment ((flag flag) env-val)
  "Retrieve FLAG from the environment."
  (declare (ignore env-val))
  ;; #### NOTE: there's no way of providing an env var /without/ a value (the
  ;; value is at least the empty string). Consequently, we decide that the
  ;; presence of the env var, regardless of its value, stands for the presence
  ;; of the flag.
  t)


;;; flag.lisp ends here
