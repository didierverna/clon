;;; constants.lisp --- SBCL Operating system interface for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sat Dec  6 20:36:38 2008
;; Last Revision: Wed Dec 17 16:53:12 2008

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

;; TIOCGWINSZ is in sys/ttycom.h on Darwin
("sys/ttycom.h" "sys/errno.h")

((:integer +tiocgwinsz+ "TIOCGWINSZ")
 (:structure winsize ("struct winsize"
		      ((unsigned 2) ws-row "unsigned short" "ws_row")
		      ((unsigned 2) ws-col "unsigned short" "ws_col")
		      ((unsigned 2) ws-xpixel "unsigned short" "ws_xpixel")
		      ((unsigned 2) ws-ypixel "unsigned short" "ws_ypixel"))))


;;; constants.lisp ends here
