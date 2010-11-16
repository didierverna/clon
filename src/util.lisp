;;; util.lisp --- General utilities

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Jun 30 17:23:36 2008
;; Last Revision: Sat Jun 12 18:21:45 2010

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
;; Miscellaneous Auxiliary Routines
;; ==========================================================================

(defmacro econd (&body clauses)
  "Like COND, but signal an error if no clause evaluates to t."
  `(cond ,@(append clauses
		   '((t (error "Fell out of ECOND clauses."))))))

(defmacro endpush (object place)
  "Like push, but at the end."
  `(setf ,place (nconc ,place (list ,object))))

(defmacro maybe-push (object place)
  "Like push, but only if OBJECT is non-nil."
  (let ((the-object (gensym "object")))
    `(let ((,the-object ,object))
      (when ,the-object (push ,the-object ,place)))))

(defmacro accumulate ((initial-value) &body body)
  "Accumulate BODY forms in a list beginning with INITIAL-VALUE.
INITIAL-VALUE is not evaluated. BODY forms are accumulated only when their
value is non-nil.
If nothing to accumulate, then return nil instead of the list of
INITIAL-VALUE."
  (let ((place (gensym "place"))
	(initial-place (gensym "initial-place")))
    `(let* ((,place (list ',initial-value))
	    (,initial-place ,place))
      ,@(mapcar (lambda (body-form)
		  `(maybe-push ,body-form ,place))
		body)
      (when (not (eq ,initial-place ,place))
	(nreverse ,place)))))

(defun beginning-of-string-p (beginning string &optional ignore-case)
  "Check that STRING starts with BEGINNING.
If IGNORE-CASE, well, ignore case."
  (let ((length (length beginning)))
    (and (>= (length string) length)
	 (funcall (if ignore-case #'string-equal #'string=)
		  beginning string :end2 length))))

(defun closest-match (match list &key ignore-case (key #'identity))
  "Return the LIST element closest to MATCH, or nil.
If IGNORE-CASE, well, ignore case.
KEY should provide a way to get a string from each LIST element."
  (let ((match-length (length match))
	(shortest-distance most-positive-fixnum)
	closest-match)
    (dolist (elt list)
      (let ((elt-string (funcall key elt))
	    distance)
	(when (and (beginning-of-string-p match elt-string ignore-case)
		   (< (setq distance (- (length elt-string) match-length))
		      shortest-distance))
	  (setq shortest-distance distance)
	  (setq closest-match elt))))
    closest-match))

(defun complete-string (beginning complete)
  "Complete BEGINNING with the rest of COMPLETE in parentheses.
For instance, completing 'he' with 'help' will produce 'he(lp)'."
  (assert (beginning-of-string-p beginning complete))
  (assert (not (string= beginning complete)))
  (concatenate 'string beginning "(" (subseq complete (length beginning)) ")"))

(defun list-to-string (list &key (key #'identity) (separator ", "))
  "Return a SEPARATOR-separated string of all LIST elements.
- KEY should provide a way to get a string from each LIST element.
- SEPARATOR is the string to insert between elements."
  (reduce (lambda (str1 str2) (concatenate 'string str1 separator str2))
	  list
	  :key key))



;; ==========================================================================
;; Key-Value Pairs Manipulation
;; ==========================================================================

(defun select-keys (keys &rest selected)
  "Return a new property list from KEYS with only SELECTED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:when (member key selected)
	:nconc (list key val)))

(defun remove-keys (keys &rest removed)
  "Return a new property list from KEYS without REMOVED ones."
  (loop :for key :in keys :by #'cddr
	:for val :in (cdr keys) :by #'cddr
	:unless (member key removed)
	:nconc (list key val)))

(defmacro replace-in-keys ((key val) keys the-key form)
  "Replace every occurrence of THE-KEY in KEYS with FORM.
At every KEYS round, KEY and VAL are bound to the current key-value pair.
FORM is evaluated each time and should return a key-value list."
  `(loop :for ,key :in ,keys :by #'cddr
    :for ,val :in (cdr ,keys) :by #'cddr
    :if (eql ,key ,the-key)
    :append ,form
    :else
    :nconc (list ,key ,val)))

;; #### NOTE: that's the typical situation where I would like a
;; destructuring-cond, but it seems difficult to do so because of the
;; standard imprecision of the reported error in case of a pattern matching
;; failure.
;; #### NOTE: I could extend this utility by supporting a global :test, or
;; even a per-replacement local one.
(defun replace-key (replacement keys)
  "Return a new property list from KEYS with REPLACEMENT.
REPLACEMENT can take the following forms:
- :KEY
  The effect is to remove :KEY from KEYS, as per REMOVE-KEYS.
- (:KEY :NEW-KEY)
  The effect is to replace :KEY with :NEW-KEY, leaving the values unchanged.
- (:KEY :NEW-KEY (VAL-OR-VALS NEW-VAL)*), with VAL-OR-VALS being
  either a value or a list of values. The effect is to replace :KEY with
  :NEW-KEY and a value matching one of the VAL-OR-VALS with the
  corresponding NEW-VAL. Values not matching any VAL-OR-VALS remain unchanged.
- (:KEY (VAL-OR-VALS :NEW-KEY NEW-VAL...)*), with VAL-OR-VALS as above. The
  effect is the same as above, but :NEW-KEY additionally depends on the
  matched value. If multiple :NEW-KEY NEW-VAL couples are provided, that many
  new keys are inserted along with their values. For values not matching any
  VAL-OR-VALS, :KEY and its value remain unchanged."
  (econd ((symbolp replacement)
	  (remove-keys keys replacement))
	 ((and (consp replacement)
	       (= (length replacement) 2)
	       (symbolp (car replacement))
	       (symbolp (cadr replacement)))
	  (destructuring-bind (old-key new-key) replacement
	    (replace-in-keys (key val) keys old-key
	      (list new-key val))))
	 ((and (consp replacement)
	       (> (length replacement) 2)
	       (symbolp (car replacement))
	       (symbolp (cadr replacement)))
	  (destructuring-bind (old-key new-key &rest replacements) replacement
	    (replace-in-keys (key val) keys old-key
	      (list new-key
		    (let ((match
			   (assoc val replacements
				  :test (lambda (val val-or-vals)
					  (if (consp val-or-vals)
					      (member val val-or-vals)
					      (eql val val-or-vals))))))
		      (if match (cadr match) val))))))
	 ((and (consp replacement)
	       (> (length replacement) 1)
	       (symbolp (car replacement)))
	  (destructuring-bind (old-key &rest replacements) replacement
	    (replace-in-keys (key val) keys old-key
	      (let ((match (assoc val replacements
				  :test (lambda (val val-or-vals)
					  (if (consp val-or-vals)
					      (member val val-or-vals)
					      (eql val val-or-vals))))))
		(if match
		    (cdr match)
		    (list key val))))))))

(defun replace-keys (keys &rest replacements)
  "Return a new property list from KEYS with REPLACEMENTS.
See REPLACE-KEY for more information on the replacement syntax."
  (let ((new-keys keys))
    (dolist (replacement replacements)
      (setq new-keys (replace-key replacement new-keys)))
    new-keys))



;; ==========================================================================
;; CLOS Utility Routines
;; ==========================================================================

;; --------------------
;; Portability wrappers
;; --------------------

(defmacro validate-superclass (class superclass)
  "Validate SUPERCLASS classes for CLASS classes."
  ;; #### PORTME.
  `(defmethod #+sbcl sb-mop:validate-superclass
	      #+cmu  mop:validate-superclass
	      #+ccl  ccl:validate-superclass
	      #+ecl  clos:validate-superclass
    ((class ,class) (superclass ,superclass))
    t))

(defun class-slots (class)
  "Return CLASS slots."
  ;; #### PORTME.
  (#+sbcl sb-mop:class-slots
   #+cmu  mop:class-slots
   #+ccl  ccl:class-slots
   #+ecl  clos:class-slots
   class))

(defun slot-definition-name (slot)
  "Return SLOT's definition name."
  ;; #### PORTME.
  (#+sbcl sb-mop:slot-definition-name
   #+cmu  mop:slot-definition-name
   #+ccl  ccl:slot-definition-name
   #+ecl  clos:slot-definition-name
   slot))


;; ----------------
;; Abstract classes
;; ----------------

(defclass abstract-class (standard-class)
  ()
  (:documentation "The ABSTRACT-CLASS class.
This is the meta-class for abstract classes."))

(defmacro defabstract (class super-classes slots &rest options)
  "Like DEFCLASS, but define an abstract class."
  (when (assoc :metaclass options)
    (error "Defining abstract class ~S: explicit meta-class option." class))
  `(defclass ,class ,super-classes ,slots ,@options
    (:metaclass abstract-class)))

(defmethod make-instance ((class abstract-class) &rest initargs)
  (declare (ignore initargs))
  (error "Instanciating class ~S: is abstract." (class-name class)))

(validate-superclass abstract-class standard-class)
(validate-superclass standard-class abstract-class)


;; ----------------
;; Instance copying
;; ----------------

(defgeneric copy-instance (instance &optional subclass)
  (:documentation "Return a copy of INSTANCE.
Copy is either an object of INSTANCE's class, or INSTANCE's SUBCLASS if given.")
  (:method (instance &optional subclass)
    "Return a copy of INSTANCE.
Both instances share the same slot values."
    (let* ((class (class-of instance))
	   (slots (class-slots class))
	   (new-instance (make-instance (or subclass class))))
      (loop :for slot :in slots
	    :when (slot-boundp instance (slot-definition-name slot))
	    :do (setf (slot-value new-instance (slot-definition-name slot))
		      (slot-value instance (slot-definition-name slot))))
      new-instance)))



;; ==========================================================================
;; Stream to file-stream conversion (thanks Nikodemus !)
;; ==========================================================================

(defgeneric stream-file-stream (stream &optional direction)
  (:documentation "Convert STREAM to a file-stream.")
  (:method ((stream file-stream) &optional direction)
    (declare (ignore direction))
    stream)
  (:method ((stream synonym-stream) &optional direction)
    (declare (ignore direction))
    (stream-file-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream) &optional direction)
    (stream-file-stream
	(case direction
	  (:input (two-way-stream-input-stream stream))
	  (:output (two-way-stream-output-stream stream))
	  (otherwise
	   (error "Cannot extract file-stream from TWO-WAY-STREAM ~A:
invalid direction: ~S"
		  stream direction)))
	direction)))



;; ==========================================================================
;; Wrappers around non ANSI features and operating system stuff
;; ==========================================================================

(defun home-directory ()
  "Return user's home directory in canonical form."
  (truename (user-homedir-pathname)))

(defun macosp ()
  "Return t if running on Mac OS."
  (string= (software-type) "Darwin"))

(defun exit (&optional (status 0))
  "Quit the current application with STATUS."
  ;; #### PORTME.
  #+sbcl (sb-ext:quit :unix-status status)
  #+cmu  (unix:unix-exit status)
  #+ccl  (ccl:quit status)
  #+ecl  (ext:quit status))

(defun cmdline ()
  "Get the current application's command-line."
  ;; #### PORTME.
  #+sbcl sb-ext:*posix-argv*
  #+cmu  lisp::lisp-command-line-list
  #+ccl  ccl::*command-line-argument-list*
  #+ecl (ext:command-args))

(defun getenv (variable)
  "Get environment VARIABLE's value. VARIABLE may be null."
  ;; #### PORTME.
  (when variable
    (#+sbcl sb-posix:getenv
     #+cmu  unix:unix-getenv
     #+ccl  ccl:getenv
     #+ecl  ext:getenv
     variable)))

(defun putenv (variable value)
  "Set environment VARIABLE to VALUE."
  ;; #### PORTME.
  #+sbcl (sb-posix:putenv  (concatenate 'string variable "=" value))
  #+cmu  (unix:unix-putenv (concatenate 'string variable "=" value))
  #+ccl  (ccl:setenv variable value)
  #+ecl  (ext:setenv variable value))

(defmacro dump (name function)
  "Dump a standalone executable named NAME starting with FUNCTION."
  ;; #### PORTME.
  #+sbcl `(sb-ext:save-lisp-and-die ,name :toplevel #',function :executable t
				    :save-runtime-options t)
  #+cmu  `(ext:save-lisp ,name :init-function #',function :executable t
			 :load-init-file nil :site-init nil
			 :print-herald nil :process-command-line nil)
  #+ccl  `(ccl:save-application ,name :toplevel-function #',function
				:init-file nil :error-handler :quit
				:prepend-kernel t)
  ;; #### NOTE: ECL works differently: it needs an entry point (i.e. actual
  ;; code to execute) instead of a main function. So we expand DUMP to just
  ;; call that function.
  #+ecl (list function))


;;; util.lisp ends here
