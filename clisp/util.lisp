;;; util.lisp --- CLISP specific utilities

;; Copyright (C) 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

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


;;; Code:

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


(defun clisp/stream-line-width (stream)
  "CLISP specific version of STREAM-LINE-WIDTH.
This function relies on CFFI."
  (multiple-value-bind (input-fd output-fd)
      (ext:stream-handles stream)
    (when output-fd
      (cffi:with-foreign-object (winsize 'winsize)
	(let ((result (cffi:foreign-funcall "ioctl"
					    :int output-fd
					    :int +tiocgwinsz+
					    :pointer winsize
					    :int)))
	  (if (= result -1)
	      (unless (= +errno+ +enotty+)
		(values nil
			(cffi:foreign-funcall "strerror"
					      :int +errno+ :string)))
	    (cffi:with-foreign-slots ((ws-col) winsize winsize)
	      ws-col)))))))


;;; util.lisp ends here
