;;; package.lisp --- Clon setup package definition

;; Copyright (C) 2021 Didier Verna

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

(in-package :cl-user)

(defpackage :net.didierverna.clon.setup
  (:documentation "The Clon setup library's package.")
  (:use :cl)
  (:import-from :named-readtables :defreadtable :in-readtable)
  (:export
    :in-readtable
    ;; from src/version.lisp:
    :*copyright-years*
    :*release-major-level* :*release-minor-level* :*release-status*
    :*release-status-level* :*release-name*
    :version
    ;; from src/configuration.lisp:
    :configuration :configure
    ;; from src/termio.lisp:
    :setup-termio))

;;; package.lisp ends here
