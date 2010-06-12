;;; clon-indent.el --- cl-indent additions

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 14:09:17 2008
;; Last Revision: Sat Jun 12 18:06:53 2010
;; Keywords:      extensions, lisp, data


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

(dolist (symbol '(select-keys remove-keys replace-keys
		  add-to
		  convert-value convert-environment
		  push-cmdline-option push-unknown-option
		  make-face))
  (put symbol 'common-lisp-indent-function 1))

(dolist (symbol '(with-winsize
		  stream-file-stream
		  make-internal-flag
		  make-internal-switch
		  make-internal-stropt
		  make-internal-lispobj
		  make-internal-path
		  make-internal-enum
		  make-internal-xswitch
		  within-group))
  (put symbol 'common-lisp-indent-function 2))

(dolist (symbol '(push-retrieved-option replace-in-keys))
  (put symbol 'common-lisp-indent-function 3))

(put 'do-options 'common-lisp-indent-function '((&whole 4 2 1) &body))


;;; clon-indent.el ends here
