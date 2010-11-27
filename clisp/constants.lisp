;;; constants.lisp --- SBCL operating system interface

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Dec  6 20:36:38 2008
;; Last Revision: Sat Jun 12 18:14:00 2010

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

(in-package :com.dvlsoft.clon)

(define "winsize_t" "struct winsize")

(include "sys/ioctl.h")
(include "errno.h")

(constant (+tiocgwinsz+ "TIOCGWINSZ") :type integer)
(constant (+enotty+     "ENOTTY")     :type integer)

(cvar ("errno" +errno+ :read-only t) :int)

(cstruct winsize "winsize_t"
	 (ws-row "ws_row" :type :unsigned-short)
	 (ws-col "ws_col" :type :unsigned-short)
	 (ws-xpixel "ws_xpixel" :type :unsigned-short)
	 (ws-ypixel "ws_ypixel" :type :unsigned-short))


;;; constants.lisp ends here
