;;; test.lisp ---

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Jun 23 21:17:15 2008
;; Last Revision: Mon Jun 23 21:17:15 2008


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(defun test ()
  (let ((synopsis (clon::make-synopsis :postfix "postfix")))
    (clon::add-to synopsis (clon::make-text :string "Demonstration of Clon."))
    (let ((grp (clon::make-group)))
      (clon::add-to grp (clon::make-text :string "Flags:"))
      (clon::add-to grp
	(clon::make-flag :short-name "h" :long-name "help"
			 :description "both names."))
      (clon::add-to grp
	(clon::make-flag :short-name "v"
			 :description "short name."))
      (clon::add-to grp
	(clon::make-flag :long-name "version"
			 :description "long name."))
      (clon::seal grp)
      (clon::add-to synopsis grp))
    (let ((grp (clon::make-group)))
      (clon::add-to grp (clon::make-text :string "Switches:"))
      (clon::add-to grp
	(clon::make-switch :short-name "d"
			   :long-name "debug"
			   :description
			   "both names, optional argument yes/no (the default)"))
      (clon::add-to grp
	(clon::make-switch :short-name "i"
			   :long-name "interactive"
			   :description
			   "both names, argument required, another name"
			   :argument-name "on(off)"
			   :argument-type :required))
      (clon::add-to grp
	(clon::make-switch :short-name "n"
			   :description "short name, whatever the argument"))
      (clon::add-to grp
	(clon::make-switch  :long-name "verbose"
			    :description
			    "long name, optional argument, yet another name"
			    :argument-name "true(false)"))
      (clon::add-to grp
	(clon::make-switch  :long-name "simulate"
			    :description "long name, optional argument"
			    :argument-type :required))
      (clon::seal grp)
      (clon::add-to synopsis grp))
    (let ((grp (clon::make-group)))
      (clon::add-to grp
	(clon::make-stropt :short-name "f"
			   :long-name "first-name"
			   :description
			   "both names, required argument (default)"))
      (clon::add-to grp
	(clon::make-stropt :short-name "F"
			   :long-name "family-name"
			   :description
			   "both names, optional argument, another name"
			   :argument-type :optional
			   :argument-name "NAME"))
      (clon::add-to grp
	(clon::make-stropt :short-name "a"
			   :description
			   "short name, required argument"))
      (clon::add-to grp
	(clon::make-stropt :short-name "c"
			   :description
			   "short name, optional argument"
			   :argument-type :optional))
      (clon::add-to grp
	(clon::make-stropt :long-name "phone"
			   :description
			   "long name, required argument"))
      (clon::add-to grp
	(clon::make-stropt :long-name "fax"
			   :description
			   "long name, optional argument"
			   :argument-type :optional))
      (clon::seal grp)
      (clon::add-to synopsis grp))
    (let ((grp (clon::make-group)))
      (let ((subgrp (clon::make-group)))
	(clon::add-to subgrp
	  (clon::make-text
	   :string "This is a demo of the group imbrication feature."))
	(clon::seal subgrp)
	(clon::add-to grp subgrp))
      (clon::seal grp)
      (clon::add-to synopsis grp))
    (clon::seal synopsis)
    (clon::make-context
     :synopsis synopsis
     :cmdline '("foo" "--deb=yes" "-vdhi" "didier" "baro" "--" "baz"))))

;;; test.lisp ends here
