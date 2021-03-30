;;; termio.lisp --- Clon termio setup

;; Copyright (C) 2015, 2021 Didier Verna

;; Author: Didier Verna <didier@didierverna.net>

;; This file is part of Clon.

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(in-package :net.didierverna.clon.setup)
(in-readtable :net.didierverna.clon)


(defun restrict-because (reason)
  "Put Clon in restricted mode because of REASON."
  (format *error-output* "~
*******************************************************************
* WARNING: ~A.~66T*
* Clon will be loaded without support for terminal autodetection. *
* See sections 2 and A.1 of the user manual for more information. *
*******************************************************************"
    reason)
  (configure :restricted t))

(defun setup-termio ()
  "Autodetect termio support.
Update Clon configuration and *FEATURES* accordingly."
  (unless (configuration :restricted)
    #+sbcl
    (handler-case (asdf:load-system :sb-grovel)
      (error () (restrict-because "unable to load SB-GROVEL")))
    #+clisp
    (cond ((member :ffi *features*)
	   (handler-case (asdf:load-system :cffi-grovel)
	     (error ()
	       (restrict-because "unable to load CFFI-GROVEL"))))
	  (t
	   (restrict-because "CLISP is compiled without FFI support")))
    #+(or allegro lispworks)
    (handler-case (asdf:load-system :cffi-grovel)
      (error ()
	(restrict-because "unable to load CFFI-GROVEL")))
    #+abcl
    (restrict-because "ABCL is in use"))
  (if (configuration :restricted)
      (setq *features* (delete :net.didierverna.clon.termio *features*))
      (pushnew :net.didierverna.clon.termio *features*)))

;;; termio.lisp ends here
