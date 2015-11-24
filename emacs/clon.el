;;; clon.el --- Source code maintenance utilities

;; Copyright (C) 2015 Didier Verna

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

;; This file contains various Emacs utilities called from the Common Lisp side
;; (Swank) when Clon is loaded / compiled with the :swank-eval-in-emacs-setup
;; option (see section A.1 of the user manual). It also requires that the
;; variable slime-enable-evaluate-in-emacs be set to t in (X)Emacs.

;; The provided information currently includes:
;; - indentation properties for some symbols.


;;; Code:

(defun clon-defindent (symbol indent)
  "Set SYMBOL's indentation to INDENT.
This function sets SYMBOL's common-lisp-indent-function property.
If INDENT is a symbol, use its indentation definition.
Otherwise, INDENT is considered as an indentation definition."
  (put symbol 'common-lisp-indent-function
       (if (symbolp indent)
	   (get indent 'common-lisp-indent-function)
	 indent)))
