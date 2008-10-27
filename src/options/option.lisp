;;; option.lisp --- Basic Option management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 14:26:44 2008
;; Last Revision: Wed Jul  2 14:26:44 2008

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
;; Error Management
;; ============================================================================

(define-condition option-error (error)
  ((option :documentation "The concerned option."
	   :type option
	   :initarg :option
	   :reader option))
  (:documentation "An error related to an option."))


;; ============================================================================
;; The Option Class
;; ============================================================================

(defabstract option (traversable)
  ((short-name :documentation "The option's short name."
	       :type (or null string)
	       :initarg :short-name
	       :initform nil
	       :reader short-name)
   (long-name :documentation "The option's long name."
	      :type (or null string)
	      :initarg :long-name
	      :initform nil
	      :reader long-name)
   (description :documentation "The option's description."
		:type (or null string)
		:initarg :description
		:initform nil
		:reader description)
   (env-var :documentation "The option's associated environment variable."
	    :type (or null string)
	    :initarg :env-var
	    :initform nil
	    :reader env-var))
  (:default-initargs
    :internal nil)
  (:documentation "The OPTION class.
This is the base class for all options."))

(defmethod initialize-instance :before
    ((option option) &key short-name long-name description internal)
  "Check validity of the name-related initargs."
  (when internal
    (assert (not (or (zerop (length long-name))
		     (zerop (length description))))))
  (unless (or short-name long-name)
    (error "Option ~A: no name given." option))
  (when (and long-name (zerop (length long-name)))
    (error "Option ~A: empty long name." option))
  (when (and short-name (zerop (length short-name)))
    (error "Option ~A: empty short name." option))
  (when (and short-name long-name (string= short-name long-name))
    (error "Option ~A: short and long names identical." option))
  ;; Short names can't begin with a dash because that would conflict with
  ;; the long name syntax.
  (when (and short-name (beginning-of-string-p "-" short-name))
    (error "Option ~A: short name begins with a dash." option))
  ;; Clon uses only long names, not short ones. But it's preferable to
  ;; reserve the prefix in both cases.
  (unless internal
    (dolist (name (list short-name long-name))
      (when (and name (or (string= name "clon")
			  (beginning-of-string-p "clon-" name)))
	(error "Option ~A: name ~S reserved by Clon." option name)))))

(defmethod initialize-instance :around
    ((option option) &rest keys &key long-name env-var internal)
  "If INTERNAL, prefix LONG-NAME with \"clon-\" and ENV-VAR with \"CLON_\"."
  (when internal
    (setq long-name (concatenate 'string "clon-" long-name))
    (setq keys (list* :long-name long-name (remove-keys keys :long-name)))
    (when env-var
      (setq env-var (concatenate 'string "CLON_" env-var))
      (setq keys (list* :env-var env-var (remove-keys keys :env-var)))))
  (apply #'call-next-method option keys))


;; -------------------------
;; Name clash check protocol
;; -------------------------

(defmethod check-name-clash ((option1 option) (option2 option))
  "Ensure that there is no name clash between OPTION1 and OPTION2."
  (unless (eq option1 option2)
    (when (and (short-name option1) (short-name option2)
	       (string= (short-name option1) (short-name option2)))
      (error "Options ~A and ~A: indentical short name ~S."
	     option1 option2 (short-name option1)))
    (when (and (long-name option1) (long-name option2)
	       (string= (long-name option1) (long-name option2)))
      (error "Options ~A and ~A: identical Long name ~S."
	     option1 option2 (long-name option1)))))


;; -----------------------
;; Option mapping protocol
;; -----------------------

(defmethod mapoptions (func (option option))
  "Call FUNC on OPTION."
  (unless (traversedp option)
    (funcall func option)))

(defmethod mapoptions :after (func (option option))
  "Mark OPTION as traversed."
  (setf (traversedp option) t))



;; ============================================================================
;; The Option Search protocol
;; ============================================================================

;; When long names are abbreviated (for instance --he instead of --help), we
;; register the command-line name like this: he(lp). In case of error report,
;; this will help the user spot where he did something wrong.
(defun complete-string (beginning complete)
  "Complete BEGINNING with the rest of COMPLETE in parentheses.
For instance, completing 'he' with 'help' will produce 'he(lp)'."
  (assert (beginning-of-string-p beginning complete))
  (assert (not (string= beginning complete)))
  (concatenate 'string beginning "(" (subseq complete (length beginning)) ")"))

(defun option-abbreviation-distance (option partial-name)
  "Return the distance between OPTION's long name and PARTIAL-NAME.
If PARTIAL-NAME does not abbreviate OPTION's long name, return
MOST-POSITIVE-FIXNUM."
  (with-slots (long-name) option
    (if (beginning-of-string-p partial-name long-name)
	(- (length long-name) (length partial-name))
	most-positive-fixnum)))

(defun match-option (option &key short-name long-name)
  "Try to match OPTION against SHORT-NAME, LONG-NAME.
If OPTION matches, return the name that matched."
  (econd (short-name
	  (when (string= short-name (short-name option))
	    short-name))
	 (long-name
	  (when (string= long-name (long-name option))
	    long-name))))

(defgeneric option-sticky-distance (option namearg)
  (:documentation ~"Try to match OPTION's short name with a sticky argument "
		  ~"against NAMEARG.
If OPTION matches, return the length of OPTION's short name; otherwise 0."))


;; ============================================================================
;; The Char Packs Protocol
;; ============================================================================

;; When examining the command-line, we first try to spot an option, then a
;; minus or plus pack, and then fall back to an unknown option. When things
;; are messed up, we prefer to try to spot options misplaced in a pack rather
;; than directly an unknown option. That's what a "potential" pack is: a pack
;; composed of single character options that are potentially misused.
;; Potential misuse means non-switches in a plus pack, options with mandatory
;; arguments in the middle of a pack and so on.
(defun potential-pack-char (option &optional as-string)
  "Return OPTION's potential pack character, if any.
If AS-STRING, return a string of that character."
  (with-slots (short-name) option
    (when (and short-name (= (length short-name) 1))
      (if as-string
	  short-name
	  (coerce short-name 'character)))))

(defgeneric minus-pack-char (option &optional as-string)
  (:documentation "Return OPTION's minus pack character, if any.
If AS-STRING, return a string of that character."))

(defgeneric plus-pack-char (option &optional as-string)
  (:documentation "Return OPTION's plus pack character, if any.
If AS-STRING, return a string of that character.")
  (:method ((option option) &optional as-string)
    "Return nil (only switches are plus-packable)."
    (declare (ignore as-string))
    nil))


;;; option.lisp ends here
