;;; version.cl --- Clon version extractor script

;; Copyright (C) 2010 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Sep 19 21:32:07 2010
;; Last Revision: Sun Nov  7 19:06:53 2010

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

(in-package :cl-user)

(require :asdf)

(asdf:operate 'asdf:load-op :com.dvlsoft.clon)
(com.dvlsoft.clon:nickname-package)

(format t "LONG_VERSION  := ~A~%~
	   SHORT_VERSION := ~A~%"
  (clon:version :long)
  (clon:version :short))

(clon:exit)


;;; version.cl ends here
