;;; termio.lisp --- Terminal-related utilities

;; Copyright (C) 2012 Didier Verna.

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>

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

(in-package :com.dvlsoft.clon)
(in-readtable :com.dvlsoft.clon)


;; Preamble C code needed for ECL's FD-LINE-WIDTH function.
#+ecl (ffi:clines "
#include <stdio.h>
#include <errno.h>
#include <sys/ioctl.h>")


;; Thanks Nikodemus!
(defgeneric stream-file-stream (stream &optional direction)
  (:documentation "Return STREAM's file-stream, or nil.")
  (:method ((stream file-stream) &optional direction)
    (declare (ignore direction))
    stream)
  (:method ((stream synonym-stream) &optional direction)
    (declare (ignore direction))
    (stream-file-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream) &optional direction)
    (stream-file-stream
     (case direction
       (:input (two-way-stream-input-stream stream))
       (:output (two-way-stream-output-stream stream))
       (otherwise
	(error "Cannot extract file-stream from TWO-WAY-STREAM ~A:
invalid direction: ~S"
	       stream direction)))
     direction))
  (:method (stream &optional direction)
    (declare (ignore direction))
    #+(or ccl ecl clisp allegro) (declare (ignore stream))
    nil))

#+ecl
(defun fd-line-width (fd)
  "Get the line width for FD (file descriptor).
Return two values:
- the line width, or -1 if it can't be computed
  (typically when FD does not denote a tty),
- an error message if the operation failed."
  (ffi:c-inline (fd) (:int) (values :int :cstring) "{
    int fd = #0;

    int cols = -1;
    char *msg = NULL;

    struct winsize window;
    if (ioctl (fd, TIOCGWINSZ, &window) == -1)
      {
	if (errno != ENOTTY)
	  msg = strerror (errno);
      }
    else
      cols = (int) window.ws_col;

    @(return 0) = cols;
    @(return 1) = msg;
}"))

;; #### NOTE: ABCL doesn't appear below because this module (termio) is never
;; loaded with it.
(defun stream-line-width (stream)
  "Get STREAM's line width.
Return two values:
- the stream's line width, or nil if it can't be computed
  (typically when the stream does not denote a tty),
- an error message if the operation failed."
  ;; #### NOTE: doing a TIOCGWINSZ ioctl here is a convenient way to both know
  ;; whether we're connected to a tty, and getting the terminal width at the
  ;; same time. In case the ioctl fails, we need to distinguish between and
  ;; ENOTTY error, which simply means that we're not connected to a terminal,
  ;; and the other which are real errors and need to be reported.
  ;; #### PORTME.
  #+sbcl
  (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (let ((file-stream (stream-file-stream stream :output)))
      (when file-stream
	(handler-case
	    (with-winsize winsize ()
	      (sb-posix:ioctl file-stream +tiocgwinsz+ winsize)
	      (winsize-ws-col winsize))
	  (sb-posix:syscall-error (error)
	    (unless (= (sb-posix:syscall-errno error) sb-posix:enotty)
	      (values nil error)))))))
  #+cmu
  (locally (declare (optimize (ext:inhibit-warnings 3)))
    (let ((file-stream (stream-file-stream stream :output)))
      (when file-stream
	(alien:with-alien ((winsize (alien:struct unix:winsize)))
	  (multiple-value-bind (success error-number)
	      (unix:unix-ioctl
	       (system:fd-stream-fd file-stream)
	       unix:tiocgwinsz
	       winsize)
	    (if success
		(alien:slot winsize 'unix:ws-col)
	      (unless (= error-number unix:enotty)
		(values nil (unix:get-unix-error-msg error-number)))))))))
  #+ccl
  (let ((fd (ccl::stream-device stream :output)))
    (when fd
      (ccl:rlet ((winsize :winsize))
	(let ((result (ccl::int-errno-call
		       (#_ioctl fd #$TIOCGWINSZ :address winsize))))
	  (if (zerop result)
	      (ccl:pref winsize :winsize.ws_col)
	    (unless (= result (- #$ENOTTY))
	      (values nil (ccl::%strerror (- result)))))))))
  #+ecl
  (when (stream-file-stream stream :output)
    (multiple-value-bind (cols msg)
	(fd-line-width (ext:file-stream-fd stream))
      (values (unless (= cols -1) cols) msg)))
  #+(or allegro clisp)
  (let ((fd #+allegro (excl::stream-output-handle stream)
	    #+clisp   (multiple-value-bind (input-fd output-fd)
			  (ext:stream-handles stream)
			(declare (ignore input-fd))
			output-fd)))
    (when fd
      (cffi:with-foreign-object (winsize 'winsize)
	(let ((result (cffi:foreign-funcall "ioctl"
			:int fd
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

;;; termio.lisp ends here
