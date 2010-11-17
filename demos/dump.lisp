;;; dump.lisp --- ECL demos dumping code

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Tue Nov 16 14:13:26 2010
;; Last Revision: Tue Nov 16 14:20:30 2010


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(require :asdf)

(defconstant +executable+
  (second (member "--" (si:command-args) :test #'string=)))
(defconstant +source+ (concatenate 'string +executable+ ".lisp"))
(defconstant +object+ (concatenate 'string +executable+ ".o"))

#-asdf2 (setf asdf:*central-registry*
	      (list* (merge-pathnames "share/common-lisp/systems/"
				      (user-homedir-pathname))
		     #p"/usr/local/share/common-lisp/systems/"
		     #p"/usr/share/common-lisp/systems/"
		     asdf:*central-registry*))

#-asdf2 (ignore-errors (asdf:operate 'asdf:load-op :asdf-binary-locations))

(asdf:operate 'asdf:load-op :com.dvlsoft.clon)

(compile-file +source+ :output-file +object+ :system-p t)
(c::build-program +executable+ :lisp-files (list +object+))

(si:exit 0)

;;; dumpecl.lisp ends here
