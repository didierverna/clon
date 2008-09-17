;;; option.lisp --- Option management for Clon

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


;; ============================================================================
;; Utilities
;; ============================================================================

;; #### NOTE: Yuck. There are places in this file, like right here, where some
;; notion of the command-line syntax is needed. This is not very nice because
;; the command-line syntax should ideally be known only to context.lisp.
;; However, since the retrieval process changes according to the option
;; classes, it is still reasonable to have it here.

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
;; The Option Class
;; ============================================================================

;; #### FIXME: make abstract
(defclass option ()
  ((short-name :documentation "The option's short name."
	       :type (or null string)
	       :initarg :short-name
	       :reader short-name)
   (long-name :documentation "The option's long name."
	      :type (or null string)
	      :initarg :long-name
	      :reader long-name)
   (description :documentation "The option's description."
		:type (or null string)
		:initarg :description
		:reader description)
   (env-var :documentation "The option's associated environment variable."
	    :type (or null string)
	    :initarg :env-var
	    :reader env-var)
   (traversedp :documentation "The option's traversal state."
	       :initform nil
	       :accessor traversedp))
  (:default-initargs
    :short-name nil
    :long-name nil
    :description nil
    :env-var nil)
  (:documentation "The OPTION class.
This is the base class for all options."))

(defmethod initialize-instance :before
    ((option option) &rest keys &key short-name long-name description env-var)
  "Check validity of the name-related initargs."
  (declare (ignore description env-var))
  (unless (or short-name long-name)
    (error "Option ~A: no name given." option))
  ;; #### FIXME: is this really necessary ? What about the day I would like
  ;; to add new syntax like -= etc ?
  ;; Empty long names are forbidden because of the special syntax -- (for
  ;; terminating options). However, it *is* possible to have *one* option with
  ;; an empty (that's different from NIL) short name. This option will just
  ;; appear as `-'. Note that this special option can't appear in a minus or
  ;; plus pack (of course :-). However (and contrary to what I had in my C
  ;; version), it can have a sticky argument if it's not a flag or a boolean.
  ;; In such a case, note that Clon will never detect unknown short options,
  ;; because it will detect the - option with a sticky argument instead.
  (when (and long-name (zerop (length long-name)))
    (error "Option ~A: empty long name." option))
  (when (and short-name long-name (string= short-name long-name))
    (error "Option ~A: short and long names identical." option))
  ;; Short names can't begin with a dash because that would conflict with
  ;; the long name syntax.
  (when (and short-name (beginning-of-string-p "-" short-name))
    (error "Option ~A: short name begins with a dash." option))
  ;; Clon uses only long names, not short ones. But it's preferable to
  ;; reserve the prefix in both cases.
  (unless (cadr (member :internal keys))
    (dolist (name (list short-name long-name))
      (when (and name (or (string= name "clon")
			  (beginning-of-string-p "clon-" name)))
	(error "Option ~A: name ~S reserved by Clon." option name)))))


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


;; ------------------
;; Traversal protocol
;; ------------------

(defmethod untraverse ((option option))
  "Mark OPTION as untraversed."
  (setf (traversedp option) nil))

(defmethod next-option ((option option))
  "Return OPTION if it is the next one in a traversal process.
If so, mark it as traversed."
  (unless (traversedp option)
    (setf (traversedp option) t)
    option))


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

(defun match-option (option &key short-name long-name partial-name)
  "Try to match OPTION against SHORT-NAME, LONG-NAME or PARTIAL-NAME.
PARTIAL-NAME is a possible long name abbreviation.
If OPTION matches, return the name that matched, possibly completed in case of
a match by PARTIAL-NAME."
  (cond (short-name
	 (when (string= short-name (short-name option))
	   short-name))
	(long-name
	 (when (string= long-name (long-name option))
	   long-name))
	(partial-name
	 (when (beginning-of-string-p partial-name (long-name option))
	   (complete-string partial-name (long-name option))))))

(defgeneric match-sticky-option (option namearg)
  (:documentation
   "Try to match OPTION's short name with a sticky argument against NAMEARG.
If option matches, return the argument part of NAMEARG."))


;; ============================================================================
;; The Char Packs  Protocol
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


;; ============================================================================
;; The Retrieval Protocol
;; ============================================================================

;; #### NOTE: Yucky yucky yuck. Design fuckage. See comment in the Utilities
;; section.
(define-condition cmdline-error (error)
  ((item :documentation "The concerned command-line item."
	 :type string
	 :initarg :item
	 :reader item))
  (:documentation "An error related to a command-line item."))

(define-condition option-error (error)
  ((option :documentation "The concerned option."
	   :type option
	   :initarg :option
	   :reader option))
  (:documentation "An error related to an option."))

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

;; #### NOTE: this macro is currently used only once.
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

(defgeneric retrieve-from-long-call
    (option cmdline-name &optional cmdline-argument cmdline)
  (:documentation "Retrieve OPTION's value from a long call.
CMDLINE-NAME is the name used on the command-line.
CMDLINE-ARGUMENT is a potentially already parsed cmdline argument.
Otherwise, CMDLINE is where to find an argument.
This function returns two values:
- the retrieved value,
- the new command-line (possibly with the first item popped if the option
  requires an argument)."))

(defgeneric retrieve-from-short-call (option &optional cmdline-argument cmdline)
  (:documentation "Retrieve OPTION's value from a short call.
CMDLINE-ARGUMENT is a potentially already parsed cmdline argument.
Otherwise, CMDLINE is where to find an argument.
This function returns two values:
- the retrieved value,
- the new command-line (possibly with the first item popped if the option
  requires an argument)."))

(defgeneric retrieve-from-plus-call (option)
  (:documentation "Retrieve OPTION's value from a plus call."))

(defgeneric retrieve-from-environment (option env-val)
  (:documentation "Retrieve OPTION's value from the environment.
ENV-VAL is the value stored in the associated environment variable.")
  (:method :before (option env-val)
     "Assert that ENV-VAL is not null."
     ;; That's because getopt is not supposed to call this function unless
     ;; there is actually somethign to retrieve.
    (assert env-val)))



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
  It defaults to nil."
  (declare (ignore short-name long-name description env-var))
  (apply #'make-instance 'flag keys))

(defun make-internal-flag (long-name description &optional env-var)
  "Make a new internal (Clon-specific) flag.
- LONG-NAME is the flag's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the flag's description.
- ENV-VAR is the flag's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil."
  (assert (not (or (zerop (length long-name))
		   (zerop (length description)))))
  (when env-var
    ;; #### NOTE: this works because the default-initargs option for env-var
    ;; is actually nil, so I don't risk missing a concatenation later.
    (setq env-var (concatenate 'string "CLON_" env-var)))
  (make-instance 'flag
    :long-name (concatenate 'string "clon-" long-name)
    :description description
    :env-var env-var
    ;; #### FIXME: I'm not quite satisfied with this design here. Other
    ;; possibilities would be to:
    ;; - temporarily set a global variable like *internal*, but /yuck/.
    ;; - temporarily define an additional :before method performing the
    ;;   clon- prefix check, but only for user-level options. Cleaner,
    ;;   but obviously more costly, although it certainly doesn't matter
    ;;   much.
    :allow-other-keys t
    :internal t))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod match-sticky-option ((flag flag) namearg)
  "Return nil (flags don't take any argument, sticky or not)."
  ;; #### NOTE: there is something a bit shaky here: this function is called
  ;; during cmdline parsing (so this is really a lexico-syntactic analysis
  ;; stage), but we return nil because of a semantic point concerning flags:
  ;; they don't take arguments. In the current scheme where the *first* sticky
  ;; option matching is returned (not the longest match), this is probably
  ;; better (another question which remains unclear is what to do when a
  ;; sticky argument leads to ambiguity). The consequence is that flags won't
  ;; ever get a cmdline-argument in retrieve-from-short-call, hence the
  ;; assertion there.
  (declare (ignore namearg))
  nil)


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
  ;; See comment about this assertion in match-sticky-option.
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


;; ============================================================================
;; The Valued Option Class
;; ============================================================================

;; #### NOTE: we should distinguish between the argument's display name, in
;; itself, and the fact that we want to actually use it. For instance, we
;; might want to display an option as just --color, but still declare that the
;; argument name is CLR so that one day, it is possible to implement escape
;; sequences like %n (for arg name) directly in the help strings. It's even
;; more than that: while the argument display name belongs to the application,
;; the fact that we want to see it probably rather belongs to the user
;; preferences. Like, an option to display help in short form or something.

;; #### WARNING: I'm not convinced by this approach for registering all new
;; valued option classes. A specific metaclass for valued options seems the
;; right place to store that information, but on the other hand, calling the
;; MOP for such a simple thing might be overkill.
(defclass valued-option-class (standard-class)
  ((option-names :documentation "The list of valued option names."
		 :initform nil
		 :accessor option-names))
  (:documentation "The VALUED-OPTION-CLASS metaclass."))

;; #### FIXME: SBCL-specific
(defmethod sb-mop:validate-superclass
    ((class standard-class) (superclass valued-option-class))
  t)

;; #### FIXME: SBCL-specific
(defmethod sb-mop:validate-superclass
    ((class valued-option-class) (superclass standard-class))
  t)

;; #### FIXME: make abstract
(defclass valued-option (option)
  ((argument-name :documentation "The option's argument display name."
		  :type string
		  :initarg :argument-name
		  :reader argument-name)
   (argument-required-p :documentation "Whether the option's argument is required."
			;; Initialization :after wards by :argument-type
			:reader argument-required-p)
   ;; #### WARNING: currently, there's no way to make a distinction between
   ;; not providing a default value, and providing a null one. I don't think
   ;; that's useful, but maybe this will change someday.
   (default-value :documentation "The option's default value."
		 :initarg :default-value
		 :reader default-value))
  (:metaclass valued-option-class)
  (:default-initargs
    :argument-name "ARG"
    :argument-type :required
    :default-value nil)
  (:documentation "The VALUED-OPTION class.
This is the base class for options accepting arguments."))

(defmethod initialize-instance :before
    ((option valued-option) &key argument-name argument-type default-value)
  "Check validity of the value-related initargs."
  (when (or (null argument-name)
	    (and argument-name (zerop (length argument-name))))
    (error "Option ~A: empty argument name." option))
  (unless (or (eq argument-type :required)
	      (eq argument-type :mandatory)
	      (eq argument-type :optional))
    (error "Option ~A: invalid argument type ~S." option argument-type))
  ;; Here, we catch and convert a potential invalid-value error into a simple
  ;; error because this check is intended for the Clon user, as opposed to the
  ;; Clon end-user. In other words, a potential error here is in the program
  ;; itself; not in the usage of the program.
  (when default-value
    (handler-case (check-value option default-value)
      (invalid-value ()
	(error "Option ~A: invalid default value ~S." option default-value)))))

(defmethod initialize-instance :after
    ((option valued-option) &key argument-name argument-type default-value)
  "Compute uninitialized OPTION slots with indirect initargs.
This currently involves the conversion of the ARGUMENT-TYPE key to the
ARGUMENT-REQUIRED-P slot."
  (declare (ignore argument-name default-value))
  (case argument-type
    ((:required :mandatory)
     (setf (slot-value option 'argument-required-p) t))
    (:optional
     (setf (slot-value option 'argument-required-p) nil))))

(defmacro defoption (class superclasses slots &rest options)
  "Wrapper around defclass for defining a new Clon valued option class."
  `(progn
    (pushnew (symbol-name ',class) (option-names (find-class 'valued-option)))
    (defclass ,class ,(cons 'valued-option superclasses) ,slots
	      ,@options)))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod match-sticky-option ((option valued-option) namearg)
  "Try to match OPTION's short name with a sticky argument against NAMEARG."
  (with-slots (short-name) option
    (when (and short-name (beginning-of-string-p short-name namearg))
      ;; This case should not happen because we always look for a complete
      ;; match before looking for a sticky match.
      (assert (not (string= namearg short-name)))
      (subseq namearg (length short-name)))))


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

;; #### FIXME: rework the error hierarchy: this one should inherit from
;; invalid-argument.
(define-condition invalid-cmdline-argument (cmdline-option-error)
  ((argument :documentation "The invalid argument."
	     :type string
	     :initarg :argument
	     :reader argument)
   (comment :documentation "An additional comment about the error."
	    :type string
	    :initarg :comment
	    :reader comment))
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

(defun restartable-cmdline-convert
    (valued-option cmdline-name cmdline-argument
     &optional (fallback-value (default-value valued-option)))
  "Restartably convert CMDLINE-ARGUMENT to VALUED-OPTION's value.
This function is used when the conversion comes from a command-line usage of
VALUED-OPTION, called by CMDLINE-NAME. FALLBACK-VALUE is what to return when
an optional argument is not provided. It defaults to VALUED-OPTION's default
value.

As well as conversion errors, this function might raise a
missing-cmdline-argument error if CMDLINE-ARGUMENT is nil and an argument is
required.

Available restarts are:
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
	     fallback-value))
    (use-fallback-value ()
      :report (lambda (stream)
		(format stream "Use fallback value (~S)." fallback-value))
      fallback-value)
    (use-default-value ()
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
       valued-option cmdline-name cmdline-argument fallback-value))))

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

(define-condition invalid-environment-value (invalid-argument)
  ()
  (:report
   (lambda (error stream)
     (format stream
	     "Environment variable ~A (for option ~S): invalid value ~S.~@[~%~A~]"
	     (env-var (option error))
	     (or (long-name (option error)) (short-name (option error)))
	     (argument error)
	     (comment error))))
  (:documentation "An invalid environment variable's value error."))

(defun environment-convert (valued-option env-val)
  "Convert ENV-VAL to VALUED-OPTION's value.
This function is used when the conversion comes from an environment variable
associated with VALUED-OPTION, and intercepts invalid-argument errors
to raise the higher level invalid-environment-value error instead."
  (handler-case (restartable-convert valued-option env-val)
    (invalid-argument (error)
      (error 'invalid-environment-value
	     :option valued-option
	     :argument env-val
	     :comment (comment error)))))

(defun read-env-val (env-var)
  "Read ENV-VAR's new value from standard input."
  (format t "Please type in a new value for the ~A environment variable:~%"
	  env-var)
  (list (read-line)))

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
      ;; #### FIXME: SBCL specific
      (sb-posix:putenv (concatenate 'string (env-var valued-option) "=" env-val))
      (restartable-environment-convert valued-option env-val))))

(defmethod retrieve-from-environment ((option valued-option) env-val)
  "Retrieve OPTION's value from the environment."
  (restartable-environment-convert option env-val))


;; ============================================================================
;; The Switch Class
;; ============================================================================

;; #### NOTE: people might want to subclass the switches in order to use other
;; true/false values (I don't know, black/white or something) and have Clon
;; still recognize them as a boolean option. But this won't work with the
;; argument-style consistency check. Think again about this.

;; A switch can appear in the following forms:
;;
;;  -(+)b, --boolean[=yes(no)]          both names, optional argument
;;  -(+)b, --boolean=yes(no)            both names, required argument
;;  -(+)b                               short name, whatever the argument
;;  --boolean[=yes(no)]                 long name,  optional argument
;;  --boolean=yes(no)                   long name,  required argument

;; Switches arguments are optional by default. This is only meaningful for
;; long-name syntax, though, because short names never take an argument (the
;; value is given by the -/+ call. When the argument is optional, omitting it
;; is equivalent to saying yes.

;; #### FIXME: I'm not very satisfied with the argument-style stuff. This
;; implies that the argument-name slot from the valued-option class is ignored
;; in switches. The design could probably be improved.
(defoption switch ()
  ((argument-style :documentation "The style of the argument (on/off etc.)."
		   :type symbol
		   :initarg :argument-style
		   :reader argument-style)
   (argument-styles :documentation "The possible argument styles."
		    :allocation :class
		    :type list
		    :initform '(:yes/no :on/off :true/false :yup/nope)
		    :accessor argument-styles)
   (yes-values :documentation "The possible 'yes' values."
	       :allocation :class
	       :type list
	       :initform '("yes" "on" "true" "yup")
	       :accessor yes-values)
   (no-values :documentation "The possible 'no' values."
	      :allocation :class
	      :type list
	      :initform '("no" "off" "false" "nope")
	      :accessor no-values))
  (:default-initargs
    ;; No :argument-name -- not used
    :argument-style :yes/no
    :argument-type :optional
    :default-value nil
    :env-var nil)
  (:documentation "The SWITCH class.
This class implements boolean options."))

(defmethod initialize-instance :before ((switch switch)
					&key short-name long-name description
					     argument-name argument-type
					     default-value env-var
					     argument-style)
  "Check validity of switch-specific initargs."
  (declare (ignore short-name long-name description
		   argument-type
		   default-value env-var))
  (unless argument-name
    (error "Argument name provided for a switch. Use argument style instead."))
  (unless (member argument-style (argument-styles switch))
    (error "Invalid switch argument style ~S." argument-style)))

(defun make-switch (&rest keys
		    &key short-name long-name description
			 argument-name argument-type
			 default-value env-var
			 argument-style)
  "Make a new switch.
- SHORT-NAME is the switch's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the switch's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the switch's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-NAME shouldn't be used (use ARGUMENT-STYLE instead).
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- DEFAULT-VALUE is the switch's default value.
  It defaults to nil.
- ENV-VAR is the switch's associated environment variable.
  It defaults to nil.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope.
  It defaults to :yes/no."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   default-value env-var
		   argument-style))
  (apply #'make-instance 'switch keys))

(defun make-internal-switch (long-name description
			     &rest keys
			     &key argument-name argument-type
				  default-value env-var
				  argument-style)
  "Make a new internal (Clon-specific) switch.
- LONG-NAME is the switch's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the switch's description.
- ARGUMENT-NAME shouldn't be used (use ARGUMENT-STYLE instead).
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- DEFAULT-VALUE is the switch's default value.
  It defaults to nil.
- ENV-VAR is the switch's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- ARGUMENT-STYLE is the switch's argument display style. It can be one of
  :yes/no, :on/off, :true/false, :yup/nope.
  It defaults to :yes/no."
  (declare (ignore argument-name argument-type default-value argument-style))
  (when env-var
    ;; #### NOTE: this works because the default-initargs option for env-var
    ;; is actually nil, so I don't risk missing a concatenation later.
    (setq env-var (concatenate 'string "CLON_" env-var)))
  (apply #'make-instance 'switch
	 :long-name (concatenate 'string "clon-" long-name)
	 :description description
	 :env-var env-var
	 :allow-other-keys t
	 :internal t
	 (remove-keys keys :env-var)))


;; -------------------------
;; Option searching protocol
;; -------------------------

(defmethod match-sticky-option ((switch switch) namearg)
  "Return nil (switches don't accept sticky arguments)."
  ;; #### NOTE: see related comment in the FLAG method.
  nil)


;; -------------------
;; Char packs protocol
;; -------------------

(defmethod minus-pack-char ((switch switch) &optional as-string)
  "Return SWITCH's minus pack character, if any."
  ;; Here, we don't need to look into the argument type (required or optional)
  ;; as for other options, because for switches, the argument type only has an
  ;;impact on long calls.
  (potential-pack-char switch as-string))

(defmethod plus-pack-char ((switch switch) &optional as-string)
  "Return SWITCH's plus pack character, if any."
  (potential-pack-char switch as-string))


;; ------------------
;; Retrieval protocol
;; ------------------

(defmethod retrieve-from-long-call
    ((switch switch) cmdline-name &optional cmdline-argument cmdline)
  "Retrieve SWITCH's value from a long call."
  ;; The difference with other valued options is that an omitted optional
  ;; argument stands for a "yes". Otherwise, it's pretty similar.
  (maybe-pop-argument cmdline switch cmdline-argument)
  (values (restartable-cmdline-convert switch cmdline-name cmdline-argument t)
	  cmdline))

(defmethod retrieve-from-short-call
    ((switch switch) &optional cmdline-argument cmdline)
  "Retrieve SWITCH's value from a short call."
  ;; See comment about this assertion in search-sticky-option.
  (assert (null cmdline-argument))
  ;; Switches don't take arguments in short form, so leave the cmdline alone
  ;; (don't mess with automatic remainder detection).
  (values t cmdline))

(defmethod retrieve-from-plus-call ((switch switch))
  "Retrieve SWITCH's value from a plus call in CMDLINE."
  nil)


;; -------------------
;; Conversion protocol
;; -------------------

(defmethod check-value ((switch switch) value)
  "Return VALUE.
All values are valid for switches: everything but nil means 'yes'."
  value)

(defmethod convert ((switch switch) argument)
  "Convert ARGUMENT to SWITCH's value.
If ARGUMENT is not valid for a switch, raise a conversion error."
  (cond ((member argument (yes-values switch) :test #'string=)
	 t)
	((member argument (no-values switch) :test #'string=)
	 nil)
	(t
	 (error 'invalid-argument
		:option switch
		:argument argument
		:comment
		(concatenate 'string
		  "Valid arguments are: "
		  (reduce (lambda (str1 str2)
			    (concatenate 'string str1 ", " str2))
			  (append (yes-values switch) (no-values switch)))
		  ".")))))


;; ============================================================================
;; The String Option Class
;; ============================================================================

;; #### NOTE: all of this applies to user-defined options as well.

;; A string option can appear in the following formats:
;;
;;   -o, --option=STR                   both names, required argument
;;   -o, --option[=STR]                 both names, optional argument
;;   -o STR                             short name, required argument
;;   -o [STR]                           short name, optional argument
;;   --option=STR                       long name,  required argument
;;   --option[=STR]                     long name,  optional argument

;; String option's arguments are required by default. In such a case, you
;; might provide the argument in the next cmdline item after either a short or
;; long name. if the argument is optional, then giving it must be done after
;; an equal sign for long names, or as a sticky argument after a short name,
;; but that's all.

(defoption stropt ()
  ()
  (:default-initargs :argument-name "STR")
  (:documentation "The STROPT class.
This class implements options the values of which are strings."))

(defun make-stropt (&rest keys
		    &key short-name long-name description
			 argument-name argument-type
			 default-value env-var)
  "Make a new string option.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- DEFAULT-VALUE is the option's default value.
  It defaults to nil.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil."
  (declare (ignore short-name long-name description
		   argument-name argument-type
		   default-value env-var))
  (apply #'make-instance 'stropt keys))

(defun make-internal-stropt (long-name description
			     &rest keys
			     &key argument-name argument-type
				  default-value env-var)
  "Make a new internal (Clon-specific) string option.
- LONG-NAME is the option's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- DEFAULT-VALUE is the option's default value.
  It defaults to nil.
- ENV-VAR is the option's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil."
  (declare (ignore argument-name argument-type default-value))
  (when env-var
    ;; #### NOTE: this works because the default-initargs option for env-var
    ;; is actually nil, so I don't risk missing a concatenation later.
    (setq env-var (concatenate 'string "CLON_" env-var)))
  (apply #'make-instance 'stropt
	 :long-name (concatenate 'string "clon-" long-name)
	 :description description
	 :env-var env-var
	 :allow-other-keys t
	 :internal t
	 (remove-keys keys :env-var)))


;; -------------------
;; Conversion protocol
;; -------------------

(defmethod check-value ((stropt stropt) value)
  "Check that VALUE is a string."
  (if (stringp value)
      value
      (error 'invalid-value
	     :option stropt
	     :value value
	     :comment "Value must be a string.")))

(defmethod convert ((stropt stropt) argument)
  "Return ARGUMENT."
  argument)


;;; option.lisp ends here
