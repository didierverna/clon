;;; sheet.lisp --- Sheet handling for Clon

;; Copyright (C) 2008 Didier Verna

;; Author:        Didier Verna <didier@lrde.epita.fr>
;; Maintainer:    Didier Verna <didier@lrde.epita.fr>
;; Created:       Sun Nov 30 16:20:55 2008
;; Last Revision: Sun Nov 30 16:20:55 2008

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

(in-package :clon)
(in-readtable :clon)


;; ==========================================================================
;; The Sheet Class
;; ==========================================================================

(defclass sheet ()
  ((output-stream :documentation "The sheet's output stream."
		  :type stream
		  :reader output-stream
		  :initarg :output-stream)
   (line-width :documentation "The sheet's line width."
	       :type (integer 1)
	       :reader line-width
	       :initarg :line-width)
   (face-tree :documentation "The sheet's face tree."
	      :reader face-tree
	      :initform (make-face-tree))
   (current-face :documentation "The current position in the face tree."
		 :accessor current-face
		 :initform nil)
   (column :documentation "The sheet's current column."
	   :type (integer 0)
	   :accessor column
	   :initform 0)
   (frames :documentation "The stack of currently open frames."
	   :type list
	   :accessor frames
	   :initform nil)
   (in-group :documentation "The current group imbrication."
	     :type (integer 0)
	     :accessor in-group
	     :initform 0)
   (last-action :documentation "The last action performed on the sheet."
		:type symbol
		:accessor last-action
		:initform :none))
  (:documentation "The SHEET class.
This class implements the notion of sheet for printing Clon help."))

(defmacro map-frames (frame (sheet &key reverse) &body body)
  "Map BODY over SHEET's frames.
If REVERSE, map in reverse order.
Bind FRAME to each frame when evaluating BODY."
  `(mapc (lambda (,frame)
	   ,@body)
    ,(if reverse
	 `(cdr (nreverse (copy-list (frames ,sheet))))
	 `(butlast (frames ,sheet)))))



;; ==========================================================================
;; Sheet Processing
;; ==========================================================================

(defstruct frame
  left-margin right-margin)

(defun left-margin (sheet)
  (frame-left-margin (car (frames sheet))))

(defun right-margin (sheet)
  (frame-right-margin (car (frames sheet))))

(defun open-frame-1 (sheet left-margin right-margin)
  "Open a new frame on SHEET between LEFT-MARGIN and RIGHT-MARGIN."
  (push (make-frame :left-margin left-margin :right-margin right-margin)
	(frames sheet)))

(defun close-frame-1 (sheet)
  (pop (frames sheet)))

(defun princ-char (sheet char)
  "Princ CHAR on SHEET's stream."
  (assert (and (char/= char #\newline) (char/= char #\tab)))
  (princ char (output-stream sheet))
  (incf (column sheet)))

(defun princ-string (sheet string)
  "Princ STRING on SHEET's stream."
  (princ string (output-stream sheet))
  (setf (column sheet) (+ (length string) (column sheet))))

(defun princ-spaces (sheet number)
  "Princ NUMBER spaces to SHEET."
  (princ-string sheet (make-string number :initial-element #\space)))

(defun reach-column (sheet column)
  "Reach COLUMN on SHEET."
  (assert (<= (column sheet) column))
  (assert (<= column (right-margin sheet)))
  (map-frames frame (sheet :reverse t)
    (unless (<= (frame-left-margin frame) (column sheet))
      (princ-spaces sheet (- (frame-left-margin frame) (column sheet)))))
  (princ-spaces sheet (- column (column sheet))))

(defun open-frame (sheet left-margin right-margin)
  (assert (frames sheet))
  (assert (>= left-margin (left-margin sheet)))
  (assert (<= (column sheet) left-margin))
  (when (<= right-margin 0)
    (setq right-margin (+ (right-margin sheet) right-margin)))
  (reach-column sheet left-margin)
  (open-frame-1 sheet left-margin right-margin))

(defun close-frame (sheet)
  (assert (frames sheet))
  (princ-spaces sheet (- (right-margin sheet) (column sheet)))
  (close-frame-1 sheet))

(defun output-newline (sheet)
  "Output a newline to SHEET's stream."
  ;; #### FIXME: don't I need to honor changes of faces ??
  (map-frames frame (sheet)
    (princ-spaces sheet (- (frame-right-margin frame) (column sheet))))
  (princ-spaces sheet (- (line-width sheet) (column sheet)))
  (terpri (output-stream sheet))
  (setf (column sheet) 0))

(defun maybe-output-newline (sheet)
  "Output a newline to SHEET if needed."
  (ecase (last-action sheet)
    ((:none :open-group)
     (values))
    ((:put-header :put-text #+():put-option #+():close-group)
     (output-newline sheet))))

(defun next-line (sheet)
  "Go to the next line and reach the proper left position."
  ;; First, close the current line
  ;; #### NOTE: Actually, output-newline would better be named close-line.
  (output-newline sheet)
  (map-frames frame (sheet :reverse t)
    ;; Next, skip frames starting at position 0 and reach the proper column,
    ;; opening the needed faces.
    (unless (zerop (frame-left-margin frame))
      (princ-spaces sheet (- (frame-left-margin frame) (column sheet))))))

(defun maybe-next-line (sheet)
  "Go to the next line if we're already past SHEET's right margin."
  (if (>= (column sheet) (right-margin sheet))
      (next-line sheet)))

;; #### FIXME: This routine does not handle special characters (the ones that
;; don't actually display anything. Since this is for short description
;; strings, this would not be normally a problem, but the current situation is
;; not totally clean.
(defun output-string (sheet string)
  "Output STRING to SHEET.
STRING is output within the current frame's bounds.
Spacing characters are honored but newlines might replace spaces when the
output reaches the rightmost bound."
  (assert (<= 0 (left-margin sheet)))
  (assert (< (left-margin sheet) (right-margin sheet)))
  (assert (and string (not (zerop (length string)))))
  ;; #### FIXME: I don't remember, but this might not work: don't I need to
  ;; honor the frames'faces here instead of blindly spacing ?? Or am I sure
  ;; I'm in the proper frame/face ?
  ;; First, adjust the tabbing.
  (if (< (column sheet) (left-margin sheet))
      (princ-spaces sheet (- (left-margin sheet) (column sheet))))
  (loop :with len = (length string)
	:with i = 0
	:while (< i len)
	:do (maybe-next-line sheet)
	(case (aref string i)
	  (#\space
	   (princ-char sheet #\space)
	   (incf i))
	  (#\tab
	   ;; #### FIXME: get a real tabsize
	   (let ((spaces (+ (- (* (+ (floor (/ (- (column sheet)
						  (left-margin sheet))
					       8))
				     1)
				  8)
			       (column sheet))
			    (left-margin sheet))))
	     (cond ((< (+ (column sheet) spaces) (right-margin sheet))
		    (princ-spaces sheet spaces))
		   (t
		    (next-line sheet)
		    (princ-spaces sheet (- (+ (column sheet) spaces)
					   (right-margin sheet))))))
	   (incf i))
	  (#\newline
	   (next-line sheet)
	   (incf i))
	  (otherwise
	   (let ((end (or (position-if
			   (lambda (char)
			     (member char '(#\space #\tab #\newline)))
			   string
			   :start i)
			  len)))
	     (cond ((or (= (column sheet) (left-margin sheet))
			(< (+ (column sheet) (- end i)) (right-margin sheet)))
		    ;; If we're at the tabbing pos, we output the word right
		    ;; here, since it couldn't fit anywhere else. Otherwise,
		    ;; we can add it here if it ends before ARRAY_LAST
		    ;; (sheet->frame_stack).right_margin
		    (princ-string sheet (subseq string i end))
		    (setq i end))
		   (t
		    (next-line sheet))))))))

(defun nesting-level (level)
  (if (or (= level 0) (= level 1)) 0 (1- level)))

(defun open-group (sheet)
  "Open a new group on SHEET."
  (let ((indent 2))
    (incf (in-group sheet))
    (maybe-output-newline sheet)
    (open-frame sheet (* (nesting-level (in-group sheet)) indent) 0)
    (setf (last-action sheet) :open-group)))

(defun close-group (sheet)
  "Close the current group on SHEET."
  (close-frame sheet)
  (decf (in-group sheet))
  (setf (last-action sheet) :close-group))

(defun open-face (sheet name)
  "Find the closest face named NAME in SHEET's face tree.
FACE can be a subface of the current face, or one up the face tree.
Return two values: the face, and whether it was found as a subface (in which
case it should be popped afterwards."
  (let ((face (cond ((null (current-face sheet))
		     (assert (eql name 'help))
		     (unless (eql (face-name (face-tree sheet)) 'help)
		       (error "Help face not found."))
		     (face-tree sheet))
		    (t
		     (find-face name (current-face sheet))))))
    (setf (current-face sheet) face)
    (open-frame-1 sheet 0 (line-width sheet))))

(defun close-face (sheet)
  "Close SHEET's current face."
  (close-frame-1 sheet)
  (setf (current-face sheet) (face-parent (current-face sheet))))

(defmacro with-face (sheet face &body body)
  `(progn
    (open-face ,sheet ,face)
    ,@body
    (close-face ,sheet)))

(defgeneric %print-help (sheet help-spec)
  (:documentation "Print HELP-SPEC on SHEET.")
  (:method (sheet (help-spec (eql #\newline)))
    (output-newline sheet))
  (:method (sheet (help-spec character))
    "Print HELP-SPEC on SHEET."
    (princ-char sheet help-spec))
  (:method (sheet (help-spec string))
    "Print HELP-SPEC on SHEET."
    (output-string sheet help-spec))
  (:method (sheet (help-spec list))
    "Print the CDR of HELP-SPEC on SHEET.
The CAR of HELP-SPEC should be a symbol naming the face to use for printing.
The HELP-SPEC items to print are separated with the contents of the face's
:item-separator property."
    (with-face sheet (car help-spec)
      (mapc (lambda (spec) (%print-help sheet spec))
	    (if (face-item-separator (current-face sheet))
		(reduce (lambda (spec1 spec2)
			  (if spec2
			      (list* spec1
				     (face-item-separator (current-face sheet))
				     spec2)
			      (list spec1)))
			(cdr help-spec)
			:from-end t :initial-value nil)
		(cdr help-spec))))))

(defun print-help (sheet help-spec)
  "Print HELP-SPEC on SHEET."
  (if (and (listp help-spec) (not (symbolp (car help-spec))))
      ;; There's already an enclosing list when help for a container is
      ;; requested directly, or when the complete help is requested, in which
      ;; case we have the list of synopsis and all synopsis items.
      (push 'help help-spec)
      (setq help-spec `(help ,help-spec)))
  (%print-help sheet help-spec))



;; ==========================================================================
;; Sheet Instance Creation
;; ==========================================================================

(defun make-sheet (&key output-stream search-path theme line-width highlight)
  "Make a new SHEET."
  (declare (ignore search-path theme highlight))
  (unless line-width
    ;; #### PORTME.
    (handler-case
	(with-winsize winsize ()
	  (sb-posix:ioctl (stream-file-stream output-stream :output)
			  +tiocgwinsz+
			  winsize)
	  (setq line-width (winsize-ws-col winsize)))
      (sb-posix:syscall-error (error)
	;; ENOTTY error should remain silent, but no the others.
	(unless (= (sb-posix:syscall-errno error) sb-posix:enotty)
	  (let (*print-escape*) (print-object error *error-output*)))
	(setq line-width 80))))
  ;; See the --clon-line-width option specification
  (assert (typep line-width '(integer 1)))
  (funcall #'make-instance 'sheet
	   :output-stream output-stream :line-width line-width))

(defun flush-sheet (sheet)
  "Flush SHEET."
  (assert (null (current-face sheet)))
  (terpri (output-stream sheet)))


;;; sheet.lisp ends here
