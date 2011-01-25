;;; util.lisp --- SBCL specific utilities

;; Copyright (C) 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

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


;;; Code:

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


(defun sbcl/stream-line-width (stream)
  "SBCL specific version of STREAM-LINE-WIDTH.
This function relies on SB-GROVEL."
  (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (handler-case
      (with-winsize winsize ()
	(sb-posix:ioctl (stream-file-stream stream :output)
			+tiocgwinsz+
			winsize)
	(winsize-ws-col winsize))
    (sb-posix:syscall-error (error)
      (unless (= (sb-posix:syscall-errno error) sb-posix:enotty)
	(values nil error)))))


;;; util.lisp ends here
