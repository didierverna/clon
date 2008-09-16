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
  (clon:declare-synopsis (:postfix "FILES...")
    (text :contents "Demonstration of Clon.")
    (group
     (text :contents "Flags:")
     (flag :short-name "h" :long-name "help" :description "both names.")
     (flag :short-name "v" :description "short name.")
     (flag :long-name "version" :description "long name."))
    (group
     (text :contents "Switches:")
     (switch :short-name "d" :long-name "debug"
	     :description "both names, optional argument yes/no (the default)"
	     :env-var "DEBUG")
     (switch :short-name "i" :long-name "interactive"
	     :description "both names, argument required, another name"
	     :argument-style :on/off
	     :argument-type :required
	     :default-value t)
     (switch :short-name "n" :description "short name, whatever the argument")
     (switch  :long-name "verbose"
	      :description "long name, optional argument, yet another name"
	      :argument-style :true/false)
     (switch  :long-name "simulate"
	      :description "long name, required argument"
	      :argument-type :required))
    (group
     (stropt :short-name "f" :long-name "first-name"
	     :description "both names, required argument (default)")
     (stropt :short-name "F" :long-name "family-name"
	     :description "both names, optional argument, another name"
	     :argument-type :optional
	     :argument-name "NAME"
	     :default-value "unknown")
     (stropt :short-name "a" :description "short name, required argument")
     (stropt :short-name "c"
	     :description "short name, optional argument"
	     :argument-type :optional)
     (stropt :long-name "phone" :description "long name, required argument")
     (stropt :long-name "fax"
	     :description "long name, optional argument"
	     :argument-type :optional))
    (group
     (group
      (text :contents "This is a demo of the group imbrication feature."))))
  "This program's synopsis .")

(defun main ()
  "This program's main function."
  ;; This context will use the POSIX command line:
  (let ((context (clon:make-context :synopsis *synopsis* :error-handler :none)))
;    (multiple-value-bind (value status source)
;	(clon:getopt context :short-name "f")
;      (print (list value status source)))
    (format t "~%~%Other options:")
    (clon:do-cmdline-options (option name value) (context :error-handler :none)
      (print (list option name value)))
    (format t "~%~%Unknown options:")
    (clon:do-unknown-options (name value) context
      (print (list name value))))
  (terpri)
  (quit))

;; #### FIXME: SBCL-specific
;(sb-ext:disable-debugger)
(save-lisp-and-die "demo" :toplevel #'main :executable t)

;;; demo.lisp ends here
