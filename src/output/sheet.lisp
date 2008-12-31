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
	     :initform 0))
  (:documentation "The SHEET class.
This class implements the notion of sheet for printing Clon help."))

(defmacro map-frames (frame (sheet &key reverse) &body body)
  "Map BODY over SHEET's frames.
If REVERSE, map in reverse order.
Bind FRAME to each frame when evaluating BODY."
  `(mapc (lambda (,frame)
	   ,@body)
    ,(if reverse
	 `(nreverse (copy-list (frames ,sheet)))
	 `(frames ,sheet))))



;; ==========================================================================
;; Sheet Processing
;; ==========================================================================

;; ----------------
;; Low level output
;; ----------------

(defun princ-char (sheet char)
  "Princ CHAR on SHEET's stream and increment the column position.
The effect of printing CHAR must be exactly to move right by one column, so
control characters, as well as newlines and tabs are forbidden here."
  ;; #### FIXME: control chars not handled.
  (assert (not (member char '(#\newline #\tab))))
  (princ char (output-stream sheet))
  (incf (column sheet)))

(defun princ-string (sheet string)
  "Princ STRING on SHEET's stream and update the column position.
The effect of printing STRING must be exactly to move right by the
corresponding string length, so control characters, as well as newlines and
tabs are forbidden here."
  ;; #### FIXME: control chars not handled.
  (assert (notany (lambda (char) (member char '(#\newline #\tab))) string))
  (princ string (output-stream sheet))
  (setf (column sheet) (+ (length string) (column sheet))))

(defun princ-spaces (sheet number)
  "Princ NUMBER spaces to SHEET's stream and update the column position."
  (princ-string sheet (make-string number :initial-element #\space)))


;; --------------
;; Logical output
;; --------------

(defstruct frame
  "The FRAME structure."
  left-margin)

(defun left-margin (sheet)
  "Return SHEET's current left-margin."
  (if (frames sheet)
      (frame-left-margin (car (frames sheet)))
      0))

(defun close-line (sheet)
  "Close all frames on SHEET's current line and go to next line."
  ;; #### NOTE: the reason why the current column might be past the line width
  ;; is that we don't do hyphenation and words might not fit anywhere
  ;; properly. See comment in output-string.
  (when (< (column sheet) (line-width sheet))
    (princ-spaces sheet (- (line-width sheet) (column sheet))))
  (terpri (output-stream sheet))
  (setf (column sheet) 0))

(defun open-line (sheet)
  "Open all frames on SHEET's current line."
  (assert (zerop (column sheet)))
  (map-frames frame (sheet :reverse t)
    (unless (zerop (frame-left-margin frame))
      (princ-spaces sheet (- (frame-left-margin frame) (column sheet))))))

(defun open-next-line (sheet)
  "Close SHEET's current line and open the next one."
  (close-line sheet)
  (open-line sheet))

(defun maybe-open-next-line (sheet)
  "Go to the next line if we're already past SHEET's line width."
  (if (>= (column sheet) (line-width sheet))
      (open-next-line sheet)))

;; #### FIXME: This routine does not handle special characters (the ones that
;; don't actually display anything. Since this is for short description
;; strings, this would not be normally a problem, but the current situation is
;; not totally clean.
(defun output-string (sheet string)
  "Output STRING to SHEET.
STRING is output within the current frame's bounds.
Spacing characters are honored but newlines might replace spaces when the
output reaches the rightmost bound."
  (assert (and string (not (zerop (length string)))))
  ;; #### FIXME: I don't remember, but this might not work: don't I need to
  ;; honor the frames'faces here instead of blindly spacing ?? Or am I sure
  ;; I'm in the proper frame/face ?
  ;; First, adjust the tabbing.
  (loop :with len = (length string) :and i = 0
	:while (< i len)
	:do (case (aref string i)
	      (#\space
	       (if (>= (column sheet) (line-width sheet))
		   ;; If we're at the end of the line, turn the space into a
		   ;; newline.
		   (open-next-line sheet)
		   ;; Otherwise, just output it.
		   (princ-char sheet #\space))
	       (incf i))
	      (#\tab
	       ;; Here, we get the real number of spaces to insert in order to
	       ;; reach the next tab position with respect to the current
	       ;; frame. #### FIXME: get a real tabsize
	       (let ((spaces (+ (- (* (ceiling (/ (- (column sheet)
						     (left-margin sheet))
						  8))
				      8)
				   (column sheet))
				(left-margin sheet))))
		 (cond ((< (+ (column sheet) spaces) (line-width sheet))
			(princ-spaces sheet spaces))
		       (t
			;; If the requested tab position is too far away, we
			;; simply go next line. There's not much that we can
			;; do to repair the layout anyway.
			(open-next-line sheet))))
	       (incf i))
	      (#\newline
	       (open-next-line sheet)
	       (incf i))
	      (otherwise
	       (let ((end (or (position-if
			       (lambda (char)
				 (member char '(#\space #\tab #\newline)))
			       string
			       :start i)
			      len)))
		 (cond ((= (column sheet) (left-margin sheet))
			;; If we're at the left-margin, we output the word
			;; right here, since it couldn't fit anywhere else.
			;; Note that since I don't do hyphenation, the word
			;; might extend past the line-width. This is bad, but
			;; this is life.
			(princ-string sheet (subseq string i end))
			(setq i end))
		       ((< (+ (column sheet) (- end i)) (line-width sheet))
			;; Otherwise, we also output the word right here if it
			;; fits on the current-line.
			(princ-string sheet (subseq string i end))
			(setq i end))
		       (t
			;; Otherwise, we have to go next line. Note that we
			;; don't actually output the word right now. This will
			;; be handled by the next LOOP iteration.
			(open-next-line sheet))))))))


;; ---------------
;; Face management
;; ---------------

(defun %open-face (sheet face)
  "Open face FACE on SHEET, and return its separator."
  (let ((left-margin
	 (let ((left-padding (left-padding face)))
	   (econd ((eq left-padding :self)
		   (column sheet))
		  #+()((eq left-padding :absolute)
		   ;; Absolute positions are OK as long as we don't roll back
		   ;; outside the enclosing frame.
		   (max left-padding (left-margin sheet)))
		  ((numberp left-padding)
		   (incf left-padding (left-margin sheet))
		   ;; In practice, it could happen that the level of
		   ;; indentation exceeds the line-width (either the theme has
		   ;; something crazy in it, or we just have too many groups)
		   ;; ... We're in trouble here, so let's just stay where we
		   ;; are.
		   (or (when (>= left-padding (line-width sheet))
			 (column sheet))
		       left-padding))))))
    (when (<= (column sheet) left-margin)
      (princ-spaces sheet (- left-margin (column sheet))))
    (push (make-frame :left-margin left-margin) (frames sheet)))
  (setf (current-face sheet) face)
  (separator (current-face sheet)))

(defun open-face (sheet name)
  "Find the closest face named NAME in SHEET's face tree.
FACE can be a subface of the current face, or one up the face tree.
Return the face separator."
  (%open-face sheet (find-face name (current-face sheet))))

(defun close-face (sheet)
  "Close SHEET's current face."
  (when (and (< (column sheet) (line-width sheet))
	     (eq (display (current-face sheet)) :block))
    (princ-spaces sheet (- (line-width sheet) (column sheet))))
  (pop (frames sheet))
  (setf (current-face sheet) (parent (current-face sheet))))

(defmacro with-face (sheet face &body body)
  `(let ((separator (open-face ,sheet ,face)))
    ,@body
    (close-face ,sheet)
    separator))

(defun open-help-face (sheet)
  "Open the help face on SHEET."
  (unless (eql (name (face-tree sheet)) 'help)
    (error "Help face not found."))
  (%open-face sheet (face-tree sheet)))

(defmacro with-help-face (sheet &body body)
  `(progn
    (open-help-face ,sheet)
    ,@body
    (close-face ,sheet)))



;; =========================================================================
;; The Print Help Protocol
;; =========================================================================

;; #### NOTE: this is where I would like more dispatch capability from CLOS.
;; Something like defmethod %print-help (sheet (help-spec (list symbol *)))
(defun %print-help-spec-items (sheet items)
  "Print help specification ITEMS on SHEET."
  (loop :for spec :on items
	:do
	(let ((separator (%print-help sheet (car spec))))
	  (when (cdr spec)
	    (when separator
	      (%print-help sheet separator))
	    (when (item-separator (current-face sheet))
	      (%print-help sheet
			   (item-separator (current-face sheet))))))))

(defgeneric %print-help (sheet help-spec)
  (:documentation "Print HELP-SPEC on SHEET.")
  (:method (sheet (help-spec (eql #\newline)))
    (open-next-line sheet)
    (values))
  (:method (sheet (help-spec character))
    "Print HELP-SPEC on SHEET."
    (%print-help sheet (make-string 1 :initial-element help-spec))
    (values))
  (:method (sheet (help-spec string))
    "Print HELP-SPEC on SHEET."
    (output-string sheet help-spec)
    (values))
  (:method (sheet (help-spec list))
    "Print the CDR of HELP-SPEC on SHEET.
The CAR of HELP-SPEC should be a symbol naming the face to use for printing.
The HELP-SPEC items to print are separated with the contents of the face's
:item-separator property."
    (with-face sheet (car help-spec)
      (%print-help-spec-items sheet (cdr help-spec)))))

(defun print-help (sheet help-spec)
  "Print HELP-SPEC on SHEET."
  (with-help-face sheet
    (%print-help-spec-items sheet
			    (if (and (listp help-spec)
				     (not (symbolp (car help-spec))))
				;; There's already an enclosing list when help
				;; for a container is requested directly, or
				;; when the complete help is requested, in
				;; which case we have the list of synopsis and
				;; all synopsis items.
				help-spec
				(list help-spec)))))



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
