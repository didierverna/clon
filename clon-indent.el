;;; clon-indent.el --- cl-indent additions for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Wed Jul  2 14:09:17 2008
;; Last Revision: Wed Jul  2 14:09:17 2008
;; Keywords:      extensions, lisp, data


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

(dolist (symbol '(select-keys remove-keys
		  add-to
		  convert-value convert-environment
		  push-cmdline-option))
  (put symbol 'common-lisp-indent-function 1))

(dolist (symbol '(make-internal-stropt make-internal-switch convert-value))
  (put symbol 'common-lisp-indent-function 2))

(put 'do-options 'common-lisp-indent-function '((&whole 4 2 1) &body))

;;; clon-indent.el ends here
