;;; demo.lisp --- Demonstration program

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Fri Aug  1 14:45:48 2008
;; Last Revision: Sat Jun 12 17:59:07 2010

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

#-asdf2 (ignore-errors (asdf:operate 'asdf:load-op :asdf-binary-locations))

(asdf:operate 'asdf:load-op :com.dvlsoft.clon)
(rename-package :com.dvlsoft.clon
		(package-name :com.dvlsoft.clon)
		(adjoin :clon (package-nicknames :com.dvlsoft.clon)
			:test #'string-equal))

(clon:defsynopsis (:postfix "FILES...")
  (text :contents
	"Demonstration of Clon (use --clon-help for built-in options).")
  (group (:header "Flags (non valued options):")
    (flag :short-name "h" :long-name "help"
	  :description "Print this help and exit."))
  (group (:header "Built-in valued option types:")
    (group (:header "String options:")
	   (stropt :short-name "n" :long-name "name"
		   :description "Set your name to NAME."
		   :argument-name "NAME"))
    (group (:header "Lisp objects:")
	   (lispobj :short-name "e" :long-name "eval"
		    :description "Evaluate EXPR."
		    :argument-name "EXPR"))
    (group (:header "Enumerations:")
	   (enum :long-name "copyright"
		 :description "Set the copyright to LICENSE.
Possible values are: none, gpl, lppl, bsd or mit."
		 :argument-name "LICENSE"
		 :argument-type :optional
		 :enum '(:none :gpl :lppl :bsd :mit)
		 :fallback-value :gpl
		 :default-value :none))
    (group (:header "Path options:")
      (path :long-name "tmpdir"
	    :description "Set the temporary directory to DIR."
	    :argument-name "DIR"
	    :type :directory
	    :default-value #p"/tmp/")
      (path :short-name "o" :long-name "output"
	    :description "Output to FILE."
	    :argument-name "FILE"
	    :type :file
	    :default-value #p"a.out")
      (path :short-name "I"
	    :description "Set the include search path to SEARCH-PATH.
SEARCH-PATH is a colon-separated list of directories. Use an empty argument to
remove all search paths."
	    :argument-name "SEARCH-PATH"
	    :type :directory-list
	    :default-value '(#p"/usr/local/share/" #p"/usr/share/")))
    (group (:header "Switches:")
      (switch :short-name "d" :long-name "debug"
	      :description "Turn debugging on or off."
	      :argument-style :on/off
	      :env-var "DEBUG"))
    (group (:header "Extended switches:")
      (xswitch :short-name "c" :long-name "connect"
	       :description "Connect to server.
Possible values are yes, no or try. If try, no errors are reported."
	       :enum '(:try)))))

(defun main ()
  "Entry point for the standalone application."
  (let ((ctx (clon:make-context)))
    (when (clon:getopt :short-name "h")
      (clon:help)
      (clon:exit))
    (format t "Program name: ~A~%~%" (clon:progname ctx))
    (format t "Options:")
    (clon:do-cmdline-options (option name value source)
      (print (list option name value source)))
    (terpri)
    (format t "Remainder: ~A~%" (clon:remainder ctx)))
  (clon:exit))

;; #### PORTME.
(save-lisp-and-die "demo"
  :toplevel #'main :executable t :save-runtime-options t)


;;; demo.lisp ends here
