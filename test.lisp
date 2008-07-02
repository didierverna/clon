;;; test.lisp ---

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Mon Jun 23 21:17:15 2008
;; Last Revision: Mon Jun 23 21:17:15 2008


;;; Commentary:

;; Contents management by FCM version 0.1.


;;; Code:

(defun test ()
  (let ((ctx (clon::make-context :postfix "files...")))
    (let ((grp (clon::make-group)))
      (clon::seal grp)
      (clon::add-to ctx grp))
    (let ((grp (clon::make-group)))
      (clon::seal grp)
      (clon::add-to ctx grp))
    (clon::seal ctx)
    ctx))



;;; test.lisp ends here
