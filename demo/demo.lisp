;;; demo.lisp --- Clon demonstration program

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Fri Aug  1 14:45:48 2008
;; Last Revision: Fri Aug  1 14:45:48 2008

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

(require :asdf)
(asdf:operate 'asdf:load-op :clon)


;; Create and fill the program synopsis:

(defvar *synopsis*
  (clon:define-synopsis synopsis (:postfix "FILES...")
    (clon:add-to synopsis (clon:make-text :string "Demonstration of Clon."))
    (clon:add-to synopsis
      (clon:declare-group
	(clon:make-text :string "Flags:")
	(clon:make-flag :short-name "h" :long-name "help"
			:description "both names.")
	(clon:make-flag :short-name "v"
			:description "short name.")
	(clon:make-flag :long-name "version"
			:description "long name.")))
    (clon:add-to synopsis
      (clon:declare-group
	(clon:make-text :string "Switches:")
	(clon:make-switch :short-name "d"
			  :long-name "debug"
			  :description
			  "both names, optional argument yes/no (the default)"
			  :env-var "DEBUG")
	(clon:make-switch :short-name "i"
			  :long-name "interactive"
			  :description
			  "both names, argument required, another name"
			  :argument-name "on(off)"
			  :argument-type :required
			  :default-value t)
	(clon:make-switch :short-name "n"
			  :description "short name, whatever the argument")
	(clon:make-switch  :long-name "verbose"
			   :description
			   "long name, optional argument, yet another name"
			   :argument-name "true(false)")
	(clon:make-switch  :long-name "simulate"
			   :description "long name, required argument"
			   :argument-type :required)))
    (clon:add-to synopsis
      (clon:declare-group
	(clon:make-stropt :short-name "f"
			  :long-name "first-name"
			  :description
			  "both names, required argument (default)")
	(clon:make-stropt :short-name "F"
			  :long-name "family-name"
			  :description
			  "both names, optional argument, another name"
			  :argument-type :optional
			  :argument-name "NAME"
			  :default-value "unknown")
	(clon:make-stropt :short-name "a"
			  :description
			  "short name, required argument")
	(clon:make-stropt :short-name "c"
			  :description
			  "short name, optional argument"
			  :argument-type :optional)
	(clon:make-stropt :long-name "phone"
			  :description
			  "long name, required argument")
	(clon:make-stropt :long-name "fax"
			  :description
			  "long name, optional argument"
			  :argument-type :optional)))
    (clon:add-to synopsis
      (clon:declare-group
	(clon:declare-group
	  (clon:make-text
	   :string "This is a demo of the group imbrication feature.")))))
  "This program's synopsis .")

(defun main ()
  "This program's main function."
  ;; This context will use the POSIX command line:
  (let ((context (clon:make-context :synopsis *synopsis*)))
    (multiple-value-bind (value status source)
	(clon:getopt context :short-name "d")
      (print (list value status source))(terpri))
    (clon:do-cmdline-options (name value status) context
      (print (list name value status))
      (terpri)))
  (quit))

;; #### FIXME: SBCL-specific
(save-lisp-and-die "demo" :toplevel #'main :executable t)

;;; demo.lisp ends here
