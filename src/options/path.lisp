;;; path.lisp --- Path options for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Oct 22 13:41:39 2008
;; Last Revision: Wed Oct 22 13:41:39 2008

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
;; The Path Option Class
;; ============================================================================

(defoption path ()
  ((argument-name ;; inherited from the VALUED-OPTION class
    :initform "PATH"))
  (:documentation "The PATH class.
This class implements options whose values are colon-separated directory names."))

(defun make-path (&rest keys
		  &key short-name long-name description env-var
		       argument-name argument-type fallback-value default-value)
  "Make a new path option.
- SHORT-NAME is the option's short name (without the dash).
  It defaults to nil.
- LONG-NAME is the option's long name (without the double-dash).
  It defaults to nil.
- DESCRIPTION is the option's description appearing in help strings.
  It defaults to nil.
- ENV-VAR is the option's associated environment variable.
  It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any."
  (declare (ignore short-name long-name description env-var
		  argument-name argument-type fallback-value default-value))
  (apply #'make-instance 'path keys))

(defun make-internal-path (long-name description
			    &rest keys
			    &key env-var argument-name
				 argument-type fallback-value default-value)
  "Make a new internal (Clon-specific) path option.
- LONG-NAME is the option's long-name, minus the 'clon-' prefix.
  (Internal options don't have short names.)
- DESCRIPTION is the options's description.
- ENV-VAR is the option's associated environment variable, minus the 'CLON_'
  prefix. It defaults to nil.
- ARGUMENT-NAME is the option's argument name appearing in help strings.
- ARGUMENT-TYPE is one of :required, :mandatory or :optional (:required and
  :mandatory are synonyms).
  It defaults to :optional.
- FALLBACK-VALUE is the option's fallback value (for missing optional
  arguments), if any.
- DEFAULT-VALUE is the option's default value, if any."
  (declare (ignore env-var
		  argument-name argument-type fallback-value default-value))
  (apply #'make-instance 'path
	 :long-name long-name
	 :description description
	 :internal t
	 keys))


;; -------------------
;; Conversion protocol
;; -------------------

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

(defmethod check-value ((path path) value)
  "Check that VALUE is a valid PATH."
  (when value
    (unless (listp value)
      (error 'invalid-value
	     :option path
	     :value value
	     :comment "Value must be a list of directory pathnames."))
    (mapc (lambda (elt)
	    (unless (pathnamep elt)
	      (error 'invalid-value
		     :option path
		     :value value
		     :comment (format nil "~S is not a pathname." elt)))
	    (unless (directory-pathname-p elt)
	      (error 'invalid-value
		     :option path
		     :value value
		     :comment (format nil "~S is not a directory pathname." elt)))
	    (when (wild-pathname-p elt)
	      (error 'invalid-value
		     :option path
		     :value value
		     :comment (format nil "~S contains wildcards." elt)))
	    (when (string= (cadr (pathname-directory elt)) "~")
	      (error 'invalid-value
		     :option path
		     :value value
		     :comment
		     (format nil "~S contains a ~~/ abbreviation." elt))))
	  value)))

(defun split-path (path)
  "Split PATH into a list of directories."
  (loop :for split-point = (position #\: path)
	:for path-elt = (subseq path 0 split-point)
	:unless (zerop (length path-elt)) :collect path-elt
	:do (setq path (when split-point (subseq path (1+ split-point))))
	:while path))

(defmethod convert ((path path) argument)
  "Convert ARGUMENT to PATH's value.
ARGUMENT must be a colon-separated list of directory names."
  (mapcar (lambda (dirname)
	    (let ((pathname (pathname dirname)))
	      (when (wild-pathname-p pathname)
		(error 'invalid-argument
		       :option path
		       :argument argument
		       :comment "Path contains wildcards."))
	      (unless (directory-pathname-p pathname)
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
			(list* :relative
			       (cddr (pathname-directory pathname)))
			:defaults pathname)
		       (user-homedir-pathname))))
	      pathname))
	  (split-path argument)))


;;; path.lisp ends here
