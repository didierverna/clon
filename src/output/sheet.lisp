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
   (highlightp :documentation "Whether to highlight SHEET's output."
	       :initarg :highlightp
	       :reader highlightp)
   (raw-face-tree :documentation "The sheet's raw face tree."
		  :initform (make-raw-face-tree)
		  :reader raw-face-tree)
   (current-raw-face :documentation "The sheet's current raw face."
		     :reader current-raw-face)
   (face-tree :documentation "The sheet's face tree."
	      :reader face-tree)
   (column :documentation "The sheet's current column."
	   :type (integer 0)
	   :accessor column
	   :initform 0)
   (frames :documentation "The stack of currently open frames."
	   :type list
	   :accessor frames
	   :initform nil))
  (:documentation "The SHEET class.
This class implements the notion of sheet for printing Clon help."))


;; ------------
;; Frame access
;; ------------

(defun push-frame (sheet frame)
  "Push a new frame to SHEET's frames."
  (push frame (frames sheet)))

(defun pop-frame (sheet)
  "Pop SHEET's current frame."
  (pop (frames sheet)))

(defun current-frame (sheet)
  "Return SHEET's current frame."
  (car (frames sheet)))

(defmacro map-frames (function (sheet &key reverse))
  "Map FUNCTION over SHEET's frames.
If REVERSE, map in reverse order."
  `(mapc ,function
    ,(if reverse
	 `(nreverse (copy-list (frames ,sheet)))
	 `(frames ,sheet))))



;; ==========================================================================
;; Sheet Processing
;; ==========================================================================

;; -------------------------
;; ISO/IEC 6429 SGR handling
;; -------------------------

(defmacro highlight-property-ecase (property value &body clauses)
  "Create an ECASE form to extract PROPERTY's VALUE escape sequence.
Each clause looks like: (PROPERTY-NAME (VALUE-OR-VALUE-LIST ESCAPE-SEQUENCE)*).
The value-matching part will itself be enclosed in an ECASE expression.
In addition, the special clause syntax (BOOLEAN <PROPERTY-NAME> <YES> <NO>)
is a shortcut for: (PROPERTY-NAME ((:on t) YES) ((:off nil) NO))."
  `(ecase ,property
    ,@(mapcar (lambda (clause)
		(if (eq (car clause) 'boolean)
		    `(,(cadr clause)
		      (ecase ,value
			((:on t)    ,(caddr  clause))
			((:off nil) ,(cadddr clause))))
		    `(,(car clause)
		      (ecase ,value
			,@(cdr clause)))))
	      clauses)))

(defun highlight-property-instance-escape-sequence (instance)
  "Return highlight property INSTANCE's escape sequence."
  (highlight-property-ecase
      (highlight-property-instance-name instance)
      (highlight-property-instance-value instance)
    ;; FAINT is not well supported
    (intensity (:bold 1) (:faint 2) ((:normal nil) 22))
    (boolean italicp 3 23)
    ;; DOUBLE is not well supported
    (underline ((:single :on t) 4) (:double 21) ((:none :off nil) 24))
    ;; RAPID is not well supported
    (blink ((:slow :on t) 5) (:rapid 6) ((:off nil) 25))
    (boolean inversep 7 27)
    (boolean concealedp 8 28)
    ;; I've seen the following two properties in some code, but I'm not sure
    ;; I've seen them work anywhere.
    (boolean crossed-out-p 9 29)
    (boolean framedp 51 54)
    (foreground (:black 30) (:red 31) (:green 32) (:yellow 33) (:blue 34)
		(:magenta 35) (:cyan 36) (:white 37) ((:reset nil) 39))
    (background (:black 40) (:red 41) (:green 42) (:yellow 43) (:blue 44)
		(:magenta 45) (:cyan 46) (:white 47) ((:reset nil) 49))))

(defun princ-highlight-property-instances (sheet instances)
  "Princ highlight proeprty INSTANCES on SHEET's stream."
  ;; #### FIXME: #\esc is not a standard name (see CLHS 13.1.7):
  (when instances
    (format (output-stream sheet) "~C[~A~{;~A~}m"
      #\esc
      (highlight-property-instance-escape-sequence (car instances))
      (mapcar #'highlight-property-instance-escape-sequence
	      (cdr instances)))))


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
  (incf (column sheet) (length string)))

(defun princ-spaces (sheet number)
  "Princ NUMBER spaces to SHEET's stream and update the column position."
  (princ-string sheet (make-string number :initial-element #\space)))

;; #### NOTE: the current column might in fact be already past the desired
;; one. For instance, since we don't do hyphenation, something too big to fit
;; in the current frame will overfull it.
(defun reach-column (sheet column)
  "Reach COLUMN on SHEET by princ'ing spaces."
  (when (< (column sheet) column)
    (princ-spaces sheet (- column (column sheet)))))


;; --------------
;; Logical output
;; --------------

(defstruct frame
  "The FRAME structure.
This structure hold layout properties used for printing."
  face
  left-margin)

(defstruct highlight-property-instance
  "The HIGHLIGHT-PROEPRTY-INSTANCE structure."
  name
  value)

(defstruct (highlight-frame (:include frame))
  "The HIGHLIGHT-FRAME structure.
This structure holds both layout and highlight properties used for printing."
  highlight-property-instances)

;; Shortcut accessors to the top frame:
(defun current-face (sheet)
  "Return SHEET's current face or nil."
  (if (frames sheet)
      (frame-face (current-frame sheet))
      nil))

(defun current-left-margin (sheet)
  "Return SHEET's current left margin or 0."
  (if (frames sheet)
      (frame-left-margin (current-frame sheet))
      0))

(defgeneric open-frame (sheet frame)
  (:documentation "Open FRAME on SHEET.")
  (:method-combination progn :most-specific-last)
  (:method progn (sheet (frame frame))
    "Reach the frame's left margin."
    (reach-column sheet (frame-left-margin frame)))
  (:method progn (sheet (frame highlight-frame))
    "Reach the frame's left margin and output its highlight properties."
    (princ-highlight-property-instances
     sheet (highlight-frame-highlight-property-instances frame))))

(defgeneric close-frame (sheet frame)
  (:documentation "Close FRAME on SHEET.")
  (:method-combination progn :most-specific-last)
  (:method progn (sheet (frame frame))
    "Reach the the end of line if FRAME's face has a :block display property."
    (when (eq (display (frame-face frame)) :block)
      (reach-column sheet (line-width sheet))))
  (:method progn (sheet (frame highlight-frame))
    "Restore the upper frame's highlight properties."
    (princ-highlight-property-instances
     sheet
     (mapcar (lambda (instance)
	       (make-highlight-property-instance
		:name (highlight-property-instance-name instance)
		:value (when (parent (frame-face frame))
			 (face-highlight-property-value
			  (parent (frame-face frame))
			  (highlight-property-instance-name instance)))))
	     (highlight-frame-highlight-property-instances frame)))))

(defun close-line (sheet)
  "Close all frames on SHEET's current line and go to next line."
  (map-frames (lambda (frame)
		(close-frame sheet frame))
      (sheet))
  (terpri (output-stream sheet))
  (setf (column sheet) 0))

(defun open-line (sheet)
  "Open all frames on SHEET's current line."
  (assert (zerop (column sheet)))
  (map-frames (lambda (frame)
		(open-frame sheet frame))
      (sheet :reverse t)))

(defun open-next-line (sheet)
  "Close SHEET's current line and open the next one."
  (close-line sheet)
  (open-line sheet))

;; #### FIXME: This routine does not handle special characters (the ones that
;; don't actually display anything. Since this is for short description
;; strings, this would not be normally a problem, but the current situation is
;; not totally clean.
(defun print-string (sheet string)
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
						     (current-left-margin
						      sheet))
						  8))
				      8)
				   (column sheet))
				(current-left-margin sheet))))
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
		 (cond ((= (column sheet) (current-left-margin sheet))
			;; If we're at the current-left-margin, we output the
			;; word right here, since it couldn't fit anywhere
			;; else. Note that since I don't do hyphenation, the
			;; word might extend past the line-width. This is bad,
			;; but this is life.
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

;; In practice, it could happen that the level of indentation exceeds the
;; line-width (either the theme has something crazy in it, or we just have too
;; many nested levels of indentation) ... We're in trouble here, so let's just
;; stay where we are.
(defun safe-padding (sheet padding)
  "Return either PADDING or SHEET's current column.
PADDING is returned when it does not exceed SHEET's line width."
  (or (when (< padding (line-width sheet))
	padding)
      (column sheet)))

(defgeneric open-face (sheet face)
  (:documentation "Open FACE on SHEET.")
  (:method (sheet (face face))
    "Create a frame for FACE and open it."
    (assert (visiblep face))
    ;; Create the new frame:
    (let ((left-margin
	   (let ((padding-spec (left-padding face)))
	     (econd ((eq padding-spec :self)
		     (column sheet))
		    ((listp padding-spec)
		     (destructuring-bind (padding relative-to &optional face-name)
			 padding-spec
		       (econd ((or (eq relative-to :absolute)
				   (and (eq relative-to :relative-to)
					(eq face-name :sheet)))
			       ;; Absolute positions are OK as long as we
			       ;; don't roll back outside the enclosing frame.
			       (max padding (current-left-margin sheet)))
			      ((and (eq relative-to :relative-to)
				    (symbolp face-name))
			       (let* ((generation
				       (parent-generation face face-name))
				      (left-margin
				       (frame-left-margin
					;; #### WARNING: we have not open the
					;; new frame yet, so decrement the
					;; generation level !!
					(nth (1- generation) (frames sheet)))))
				 (incf padding left-margin)
				 (safe-padding sheet padding))))))
		    ((numberp padding-spec)
		     (incf padding-spec (current-left-margin sheet))
		     (safe-padding sheet padding-spec))))))
      (push-frame sheet
		  (if (highlightp sheet)
		      (let ((highlight-property-instances
			     (loop :for property :in *highlight-properties*
				   :when (face-highlight-property-set-p
					  face property)
				   :collect (make-highlight-property-instance
					     :name property
					     :value
					     (face-highlight-property-value
					      face property)))))
			(make-highlight-frame :face face
					      :left-margin left-margin
					      :highlight-property-instances
					      highlight-property-instances))
		      (make-frame :face face  :left-margin left-margin))))
    ;; Open the new frame:
    (open-frame sheet (current-frame sheet)))
  (:method (sheet (name symbol))
    "Find a face named NAME in SHEET's face tree and open it."
    (open-face sheet (find-face (current-face sheet) name))))

(defun close-face (sheet)
  "Close SHEET's current face."
  (close-frame sheet (current-frame sheet))
  (pop-frame sheet))

(defmacro with-face (sheet name &body body)
  "Evaluate BODY with SHEET's current face set to the one named NAME."
  `(progn
    (open-face ,sheet ,name)
    ,@body
    (close-face ,sheet)))



;; =========================================================================
;; The Print Help Protocol
;; =========================================================================

(defun help-spec-items-will-print (face items)
  "Return t if at least one of ITEMS will print under FACE."
  (and (visiblep face)
       (some (lambda (help-spec)
	       (help-spec-will-print face help-spec))
	     items)))

(defgeneric help-spec-will-print (face help-spec)
  (:documentation
   "Return t if HELP-SPEC will print under FACE.")
  (:method (face help-spec)
    "Basic help specifications (chars, strings etc) do print."
    t)
  (:method (face (help-spec list))
    "Return t if HELP-SPEC's items will print under HELP-SPEC's face."
    (let ((subface (find-face face (car help-spec))))
      (help-spec-items-will-print subface (cdr help-spec)))))

(defgeneric get-separator (face help-spec)
  (:documentation "Get HELP-SPEC's separator under FACE.")
  (:method (face help-spec)
    "Basic help specifications (chars, strings etc) don't provide a separator."
    nil)
  (:method (face (help-spec list))
    "Return the separator of HELP-SPEC's face."
    (separator (find-face face (car help-spec)))))

;; #### NOTE: this is where I would like more dispatch capability from CLOS.
;; Something like defmethod print-help-spec (sheet (help-spec (list symbol *)))
(defun print-help-spec-items (sheet items)
  "Print all help specification ITEMS on SHEET with the current face."
  (loop :for help-specs :on items
	:do
	(when (help-spec-will-print (current-face sheet) (car help-specs))
	  (print-help-spec sheet (car help-specs))
	  (when (and (cdr help-specs)
		     (help-spec-items-will-print (current-face sheet)
						 (cdr help-specs)))
	    (let ((separator (get-separator (current-face sheet)
					    (car help-specs))))
	      (if separator
		  (print-help-spec sheet separator)
		  (if (item-separator (current-face sheet))
		      (print-help-spec sheet
				       (item-separator
					(current-face sheet))))))))))

(defgeneric print-help-spec (sheet help-spec)
  (:documentation "Print HELP-SPEC on SHEET.")
  (:method (sheet (char character))
    "Print CHAR on SHEET with the current face."
    (print-help-spec sheet (make-string 1 :initial-element char)))
  (:method (sheet (char-vector simple-vector))
    "Print CHAR-VECTOR on SHEET with the current face."
    (print-help-spec sheet (coerce char-vector 'string)))
  (:method (sheet (string string))
    "Print STRING on SHEET with the current face."
    (print-string sheet string))
  (:method (sheet (help-spec list))
    "Open HELP-SPEC's face and print all of its items with it."
    (with-face sheet (car help-spec)
      (print-help-spec-items sheet (cdr help-spec)))))

(defun print-help (sheet help)
  "Open the toplevel help face and print HELP on SHEET with it."
  (let ((items
	 (if (and (listp help) (not (symbolp (car help))))
	     ;; There's already an enclosing list when help for a container is
	     ;; requested directly, or when the complete help is requested, in
	     ;; which case we have the list of synopsis and all synopsis
	     ;; items.
	     help
	     (list help))))
    (when (help-spec-items-will-print (face-tree sheet) items)
      (open-face sheet (face-tree sheet))
      (print-help-spec-items sheet items)
      (close-face sheet))))



;; ==========================================================================
;; Sheet Instance Creation
;; ==========================================================================

;; #### WARNING: remove that after upgrading SBCL
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
;; #### NOTE: I need to bind output-stream here (which is early) because it is
;; required to do the TIOCGWINSZ business.
(defmethod initialize-instance :around
    ((sheet sheet) &rest keys &key (output-stream *standard-output*)
				  line-width
				  (highlight :auto))
  "Handle unset line width and AUTO highlight according to OUTPUT-STREAM."
  (when (or (not line-width) (eq highlight :auto))
    ;; #### NOTE: it is somewhat abusive to TIOCGWINSZ even if LINE-WIDTH is
    ;; specified, but this allows us to handle the ENOTTY error, and possibly
    ;; turn highlighting off if is it set to :AUTO.
    (handler-case
	;; #### PORTME.
	(with-winsize winsize ()
	  (sb-posix:ioctl (stream-file-stream output-stream :output)
			  +tiocgwinsz+
			  winsize)
	  (unless line-width
	    (setq line-width (winsize-ws-col winsize)))
	  (when (eq highlight :auto)
	    (setq highlight t)))
      (sb-posix:syscall-error (error)
	;; ENOTTY error should remain silent, but no the others.
	(unless (= (sb-posix:syscall-errno error) sb-posix:enotty)
	  (let (*print-escape*) (print-object error *error-output*)))
	(unless line-width
	  (setq line-width 80))
	(when (eq highlight :auto)
	  (setq highlight nil)))))
  (apply #'call-next-method sheet
	 :output-stream output-stream
	 :line-width line-width
	 :highlightp highlight
	 (remove-keys keys :output-stream :line-width :highlight)))

(defun read-face-tree (pathname)
  "Read a face tree from PATHNAME."
  (make-face-tree
   (list* 'toplevel
	  (with-open-file (stream pathname)
	    (let ((*package* (find-package :clon)))
	      (loop :for item := (read stream nil stream)
		    :if (eql item stream)
		    :return items
		    :else
		    :collect item :into items))))))

(defun try-read-face-tree (pathname)
  "Read a face tree from PATHNAME if it exists or return nil."
  (when (open pathname :direction :probe)
    (read-face-tree pathname)))

(defun try-read-theme (pathname)
  "Read a theme from PATHNAME or PATHNAME.cth if it exists or return nil."
  (or (try-read-face-tree pathname)
      (unless (string= (pathname-type pathname) "cth")
	(try-read-face-tree (merge-pathnames pathname
					     (make-pathname :type "cth"))))))

(defmethod initialize-instance :after ((sheet sheet) &key theme search-path)
  "Finish initialization of SHEET.
This involves:
- Initializing SHEET's current raw face,
- computing SHEET's face tree from THEME and SEARCH-PATH."
  (setf (slot-value sheet 'current-raw-face) (raw-face-tree sheet))
  (setf (slot-value sheet 'face-tree)
	(or (cond ((and theme
			(or (not search-path)
			    (pathname-directory theme)))
		   (try-read-theme theme))
		  (theme
		   (setq theme
			 (merge-pathnames theme
					  (make-pathname
					   :directory `(:relative
							,(if (mac-os-x-p)
							     "Themes"
							     "themes")))))
		   (loop :for path :in search-path
			 :for face-tree := (try-read-theme
					    (merge-pathnames theme path))
			 :until face-tree
			 :finally (return face-tree))))
	    (make-raw-face-tree))))

(defun make-sheet
    (&rest keys &key output-stream search-path theme line-width highlight)
  "Make a new SHEET."
  (declare (ignore output-stream search-path theme line-width highlight))
  (apply #'make-instance 'sheet keys))

(defun flush-sheet (sheet)
  "Flush SHEET."
  (assert (null (current-face sheet)))
  (terpri (output-stream sheet)))


;;; sheet.lisp ends here
