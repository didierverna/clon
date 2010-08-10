;;; path.lisp --- Path options

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Oct 22 13:41:39 2008
;; Last Revision: Sat Jun 12 18:24:09 2010

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
;; The Path Option Class
;; ==========================================================================

(defoption path ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "PATH")
   (nullablep ;; inherited from the VALUED-OPTION class
    ;; Note that this doesn't really matter, as empty paths are already
    ;; handled below.
    :initform t)
   (path-type :documentation "The path type."
	      :initarg :type
	      :initform nil
	      :reader path-type))
  (:documentation "The PATH class.
This class implements options whose values are (colon-separated lists of)
pathnames."))


;; -------------------
;; Conversion protocol
;; -------------------

;; #### FIXME: (not the right place here). We should always call check-value
;; on the converted values. That would make convert methods simpler sometimes
;; because they would not have to duplicate the work in check-value.

;; Some parts of the code below are stolen or adapted from Peter Seibel's
;; Practical Common Lisp book, Chapter 15: A Portable Pahtname Library.

(defun pathname-component-null-p (component)
  "Return true if COMPONENT is either null or :unspecific."
  (or (null component) (eql component :unspecific)))

(defun directory-pathname-p  (pathname)
  "Return true if PATHNAME denotes a directory."
  (and (pathname-component-null-p (pathname-name pathname))
       (pathname-component-null-p (pathname-type pathname))
       pathname))

;; Value check subprotocol
(defmethod check-value ((path path) value)
  "Check that VALUE is a valid PATH."
  (flet ((check-pathname (pathname &key as-file as-directory)
	   "Check that PATHNAME is valid.
If AS-FILE, ensure that it denotes a file.
If AS-DIRECTORY, ensure that it denotes a directory."
	   (unless (pathnamep pathname)
	     (error 'invalid-value
		    :option path
		    :value value
		    :comment (format nil "~S is not a pathname." pathname)))
	   (when (and as-file (directory-pathname-p pathname))
	     (error 'invalid-value
		    :option path
		    :value value
		    :comment (format nil "~S denotes a directory." pathname)))
	   (when (and as-directory (not (directory-pathname-p pathname)))
	     (error 'invalid-value
		    :option path
		    :value value
		    :comment (format nil "~S does not denote a directory."
			       pathname)))
	   (when (wild-pathname-p pathname)
	     (error 'invalid-value
		    :option path
		    :value value
		    :comment (format nil "~S contains wildcards." pathname)))
	   (when (string= (cadr (pathname-directory pathname)) "~")
	     (error 'invalid-value
		    :option path
		    :value value
		    :comment
		    (format nil "~S contains a ~~/ abbreviation." pathname)))
	   pathname))
    (ecase (path-type path)
      (:file
       (check-pathname value :as-file t))
      (:directory
       (check-pathname value :as-directory t))
      (:file-list
       (mapc (lambda (pathname)
	       (check-pathname pathname :as-file t))
	     value)
       value)
      (:directory-list
       (mapc (lambda (pathname)
	       (check-pathname pathname :as-directory t))
	     value)
       value)
      ((nil)
       (mapc (lambda (pathname)
	       (check-pathname pathname))
	     value)
       value))))

(defun split-path (path)
  "Split PATH into a list of directories."
  (loop :for split-point = (position #\: path)
	:for path-elt = (subseq path 0 split-point)
	:unless (zerop (length path-elt)) :collect path-elt
	:do (setq path (when split-point (subseq path (1+ split-point))))
	:while path))

(defmethod convert ((path path) argument)
  "Convert ARGUMENT to PATH's value."
  (flet ((string-pathname (string &key as-file as-directory)
	   "Make a pathname from STRING.
If AS-FILE, make sure the resulting pathname does not denote a directory.
If AS-DIRECTORY, make sure the resulting pathname denotes a directory."
	   (let ((pathname (pathname string)))
	     (when (wild-pathname-p pathname)
	       (error 'invalid-argument
		      :option path
		      :argument argument
		      :comment "Path contains wildcards."))
	     (when (and as-file (directory-pathname-p pathname))
	       (error 'invalid-argument
		      :option path
		      :argument argument
		      ;; #### FIXME: misleading error message when a single
		      ;; path is requested.
		      :comment "Path is or contains a directory."))
	     ;; #### NOTE: instead of forcing users to end their directories
	     ;; with a slash (and hence triggering an error here if it
	     ;; doesn't), we simply convert a normal pathname into a directory
	     ;; one.
	     (when (and as-directory (not (directory-pathname-p pathname)))
	       (setq pathname
		     (make-pathname
		      :directory (append (or (pathname-directory pathname)
					     (list :relative))
					 (list (file-namestring pathname)))
		      :name nil
		      :type nil
		      :defaults pathname)))
	     (when (string= (cadr (pathname-directory pathname)) "~")
	       (setq pathname
		     (merge-pathnames
		      (make-pathname
		       :directory
		       (list* :relative (cddr (pathname-directory pathname)))
		       :defaults pathname)
		      (user-homedir-pathname))))
	     pathname)))
    (ecase (path-type path)
      (:file
       (string-pathname argument :as-file t))
      (:directory
       (string-pathname argument :as-directory t))
      (:file-list
       (mapcar (lambda (filename)
		 (string-pathname filename :as-file t))
	       (split-path argument)))
      (:directory-list
       (mapcar (lambda (dirname)
		 (string-pathname dirname :as-directory t))
	       (split-path argument)))
      ((nil)
       (mapcar #'string-pathname
	       (split-path argument))))))



;; ==========================================================================
;; Path Instance Creation
;; ==========================================================================

(defun make-path (&rest keys
		  &key short-name long-name description
		       argument-name argument-type
		       env-var fallback-value default-value
		       nullablep type hidden)
  "Make a new path option.
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
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- NULLABLEP indicates whether this option accepts nil as a value.
- TYPE is the pathname type. It can be one of :file, :directory, :file-list,
  :directory-list or nil meaning that everything is allowed.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore short-name long-name description env-var
		  argument-name argument-type fallback-value default-value
		  nullablep type hidden))
  (apply #'make-instance 'path keys))

(defun make-internal-path (long-name description
			    &rest keys
			    &key argument-name argument-type
				 env-var fallback-value default-value
				 nullablep type hidden)
  "Make a new internal (Clon-specific) path option.
- LONG-NAME is the option's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- ENV-VAR is the option's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any.
- NULLABLEP indicates whether this option accepts nil as a value.
- TYPE is the pathname type. It can be one of :file, :directory, :file-list,
  :directory-list or nil meaning that everything is allowed.
- When HIDDEN, the option doesn't appear in help strings."
  (declare (ignore argument-name argument-type
		   env-var fallback-value default-value
		   nullablep type hidden))
  (apply #'make-instance 'path
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;;; path.lisp ends here
