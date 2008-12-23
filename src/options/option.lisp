;;; option.lisp --- Basic Option management for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 14:26:44 2008
;; Last Revision: Wed Nov  5 14:30:09 2008

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


;; ==========================================================================
;; The Option Class
;; ==========================================================================

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


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((option option))
  "OPTION is a terminal object: just return it."
  option)


;; ----------------
;; Display protocol
;; ----------------

(defmethod display ((option option))
  "Return OPTION's display specification."
  (let* ((short-name-help
	  (when (short-name option)
	    `(short-name ,(format nil "-~A" (short-name option)))))
	 (long-name-help
	  (when (long-name option)
	    `(long-name ,(format nil "--~A" (long-name option)))))
	 (syntax-help
	  (let ((syn (list 'syntax)))
	    (maybe-push short-name-help syn)
	    (maybe-push long-name-help syn)
	    (nreverse syn)))
	 (environment-help
	  (when (env-var option)
	    `(environment ,(format nil "Environment: ~A" (env-var option)))))
	 (description-help
	  (let ((desc (list 'description)))
	    (maybe-push (description option) desc)
	    (maybe-push environment-help desc)
	    (nreverse desc)))
	 (option-help
	  (let ((opt (list 'option)))
	    (maybe-push syntax-help opt)
	    (maybe-push description-help opt))))
    (nreverse option-help)))



;; ==========================================================================
;; Error Management
;; ==========================================================================

(define-condition option-error (error)
  ((option :documentation "The concerned option."
	   :type option
	   :initarg :option
	   :reader option))
  (:documentation "An error related to an option."))



;; ==========================================================================
;; The Name Clash Check Protocol
;; ==========================================================================

(defgeneric check-name-clash (item1 item2)
  (:documentation ~"Check for name clash between ITEM1's options "
		  ~"and ITEM2's options.")
  (:method (item1 (text text))
    "Do nothing (no name clash with a text object."
    (values))
  (:method ((text text) item2)
    "Do nothing (no name clash with a text object."
    (values))
  ;; #### NOTE: currently, name clashes are considered on short and long names
  ;; independently. That is, it is possible to have a short name identical to
  ;; a long one, although I don't see why you would want to do that, and I
  ;; should probably prohibit it altogether.
  (:method ((option1 option) (option2 option))
    "Ensure that there is no name clash between OPTION1 and OPTION2."
    (unless (eq option1 option2)
      (when (and (short-name option1) (short-name option2)
		 (string= (short-name option1) (short-name option2)))
	(error "Options ~A and ~A: indentical short name ~S."
	       option1 option2 (short-name option1)))
      (when (and (long-name option1) (long-name option2)
		 (string= (long-name option1) (long-name option2)))
	(error "Options ~A and ~A: identical Long name ~S."
	       option1 option2 (long-name option1))))))



;; ==========================================================================
;; The Option Search protocol
;; ==========================================================================

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
If OPTION matches, return the length of OPTION's short name; otherwise 0.")
  ;; #### NOTE: this method currently only applies to flags.
  (:method ((option option) namearg)
    "Return 0 (non-valued options don't take any argument, sticky or not)."
    ;; #### NOTE: the consequence of this method returning 0 is that
    ;; non-valued options (i.e. flags) won't ever get a cmdline-argument in
    ;; retrieve-from-short-call, hence the assertion there.
    (declare (ignore namearg))
    0))



;; ==========================================================================
;; The Char Packs Protocol
;; ==========================================================================

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
If AS-STRING, return a string of that character.")
  ;; #### NOTE: this method currently only applies to flags.
  (:method ((option option) &optional as-string)
    "Return OPTION's potential pack character."
    ;; Since non-valued options don't take any argument, being minus-packable
    ;; is the same as being potentially packable.
    (potential-pack-char option as-string)))

(defgeneric plus-pack-char (option &optional as-string)
  (:documentation "Return OPTION's plus pack character, if any.
If AS-STRING, return a string of that character.")
  (:method ((option option) &optional as-string)
    "Return nil (only the switch hierarchy is plus-packable)."
    (declare (ignore as-string))
    nil))



;; ==========================================================================
;; Option Instance Creation
;; ==========================================================================

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


;;; option.lisp ends here
