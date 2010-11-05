#! /usr/local/bin/sbcl --script

;;; generate.sh --- Clon version extractor script

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Sep 19 21:32:07 2010
;; Last Revision: Tue Oct  5 16:00:08 2010

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
#-asdf2 (setf asdf:*central-registry*
	      (list* #p"./"
		     #p"../"
		     (merge-pathnames "share/common-lisp/systems/"
				      (user-homedir-pathname))
		     #p"/usr/local/share/common-lisp/systems/"
		     #p"/usr/share/common-lisp/systems/"
		     asdf:*central-registry*))
#-asdf2 (ignore-errors (asdf:operate 'asdf:load-op :asdf-binary-locations))

(handler-case (asdf:operate 'asdf:load-op :com.dvlsoft.clon)
  (error ()
     (format t "LONG_VERSION := unknown~%SHORT_VERSION := unknown~%")
     ;; #### PORTME.
     #+sbcl (sb-ext:quit)
     #+cmu  (unix:unix-exit)))

(format t "LONG_VERSION  := ~A~%~
	   SHORT_VERSION := ~A~%"
  (com.dvlsoft.clon:version :long)
  (com.dvlsoft.clon:version :short))



;;; Local Variables:
;;; mode: lisp
;;; End:

;;; generate.sh ends here
