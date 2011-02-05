;;; advanced.lisp --- Advanced usage demonstration program

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Clon.

;; Redistribution and use in source or binary form, with or without
;; modification, are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.

;; Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; Neither the names of the authors or copyright holders, nor the names of any
;; contributor or organization may be used to endorse or promote products
;; derived from Clon without specific prior written permission.

;; CLON IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.


;;; Commentary:

;; Contents management by FCM version 0.1.

;; This demonstration program shows how to use multiple synopsis, contexts and
;; (virtual) command-lines in the same application in order to manage more
;; complex command-line syntax where options and non-options parts can be
;; freely intermixed. See section 5 "Advanced Usage" in the Clon User Manual.

;; #### NOTE: some trickery is needed below in order to make this code
;; ECL-compliant, due to ECL's specific way of generating executables. This
;; includes:
;; - setting *load-verbose* to nil,
;; - passing a nil :verbose flag to asdf:operate,
;; - wrapping nickname-package in an eval-when form.
;; None of these tweaks are needed for the other compilers.


;;; Code:

(in-package :cl-user)

(setq *load-verbose* nil)

(require :asdf
	 #-(or sbcl cmu ccl ecl)
	 '(#p"/usr/local/share/common-lisp/source/asdf/asdf.lisp"))

#-asdf2 (setf asdf:*central-registry*
	      (list* (merge-pathnames "share/common-lisp/systems/"
				      (user-homedir-pathname))
		     #p"/usr/local/share/common-lisp/systems/"
		     #p"/usr/share/common-lisp/systems/"
		     asdf:*central-registry*))

#-asdf2 (ignore-errors (asdf:operate 'asdf:load-op :asdf-binary-locations))

(asdf:operate 'asdf:load-op :com.dvlsoft.clon :verbose nil)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (com.dvlsoft.clon:nickname-package))

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

(clon:dump "advanced" main)


;;; advanced.lisp ends here
