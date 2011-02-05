;;; constants.lisp --- CLISP / CFFI operating system interface

;; Copyright (C) 2010, 2011 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

;; This file is part of Clon.

;; Redistribution and use in source or binary form, with or without
;; modification, are permitted provided that the following conditions are met:

;; Redistributions of source code must retain the above copyright notice, this
;; list of conditions and the following disclaimer.

;; Redistributions in binary form must reproduce the above copyright notice,
;; this list of conditions and the following disclaimer in the documentation
;; and/or other materials provided with the distribution.

;; Neither the names of the authors or copyright holders, nor the names of any
;; contributor or organization may be used to endorse or promote products
;; derived from Clon without specific prior written permission.

;; CLON IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY
;; EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.


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
