;;; advanced.lisp --- Advanced usage demonstration program

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Oct 20 15:40:18 2010
;; Last Revision: Wed Oct 20 16:31:06 2010

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

(in-package :cl-user)

(require :asdf)
#-asdf2 (setf asdf:*central-registry*
	      (list* #p"../"
		     (merge-pathnames "share/common-lisp/systems/"
				      (user-homedir-pathname))
		     #p"/usr/local/share/common-lisp/systems/"
		     #p"/usr/share/common-lisp/systems/"
		     asdf:*central-registry*))
#+asdf2 (asdf:initialize-source-registry
	 `(:source-registry
	   (:tree ,(merge-pathnames "science/src/common-lisp/"
				    (user-homedir-pathname)))
	   (:tree "/usr/local/src/common-lisp/")
	   (:tree "/usr/share/common-lisp/source/")
	   :inherit-configuration))

#-asdf2 (ignore-errors (asdf:operate 'asdf:load-op :asdf-binary-locations))

(asdf:operate 'asdf:load-op :com.dvlsoft.clon)
(rename-package :com.dvlsoft.clon
		(package-name :com.dvlsoft.clon)
		(adjoin :clon (package-nicknames :com.dvlsoft.clon)
			:test #'string-equal))

(clon:defsynopsis (:postfix "cmd [OPTIONS]")
  (text :contents "Available commands: push pull.
Use 'cmd --help' to get command-specific help.")
  (flag :short-name "h" :long-name "help"
	:description "Print this help and exit.")
  (switch :short-name "d" :long-name "debug"
	  :description "Turn debugging on or off."
	  :argument-style :on/off
	  :env-var "DEBUG"))

(defconstant +push-synopsis+
    (clon:defsynopsis (:make-default nil)
      (text :contents "Push local changes to the remote server.")
      (flag :short-name "h" :long-name "help"
	    :description "Print this help and exit.")
      (flag :short-name "d" :long-name "dry-run"
	    :description "Fake the push operation.")
      (stropt :long-name "remote"
	      :argument-name "SERVER"
	      :description "Use SERVER instead of default remote."))
  "The synopsis for the PUSH operation.")

(defconstant +pull-synopsis+
    (clon:defsynopsis (:make-default nil)
      (text :contents "Pull remote changes to the local server.")
      (flag :short-name "h" :long-name "help"
	    :description "Print this help and exit.")
      (flag :short-name "d" :long-name "dry-run"
	    :description "Fake the push operation.")
      (switch :long-name "update"
	      :default-value t
	      :description "Also update the working directory."))
  "The synopsis for the PULL operation.")


(defun main ()
  "Entry point for the standalone application."
  (clon:make-context)
  (cond ((clon:getopt :short-name "h")
	 (clon:help))
	(t
	 (unless (clon:remainder)
	   (format t "Missing command.~%")
	   (clon:exit 1))
	 (clon:make-context
	  :synopsis (cond ((string= (first (clon:remainder)) "push")
			   +push-synopsis+)
			  ((string= (first (clon:remainder)) "pull")
			   +pull-synopsis+)
			  (t
			   (format t "Unknown command.~%")
			   (clon:exit 1)))
	  :cmdline (clon:remainder))
	 (cond ((clon:getopt :short-name "h")
		(clon:help))
	       (t
		(format t "Command name: ~A~%~%" (clon:progname))
		(format t "Options:")
		(clon:do-cmdline-options (option name value source)
		  (print (list option name value source)))
		(terpri)
		(format t "Remainder: ~A~%" (clon:remainder))))))
  (clon:exit))

;; #### PORTME.
(save-lisp-and-die "advanced"
  :toplevel #'main :executable t :save-runtime-options t)


;;; demo.lisp ends here
