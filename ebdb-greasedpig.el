;;; ebdb-greasedpig.el --- Naive attempts at persistence speedups  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The classes and methods below are an attempt at making
;; `eieio-persistent' perform a little better (particularly when
;; reconstructing objects at load time), and at shrinking the size of
;; the persistence files.  If this works out, the hope is that parts
;; of it may replace the base behavior of `eieio-persistent'.

;; Right now, loading is slow in part because there's a little too
;; much typechecking and verification going on, and also simply
;; because `make-instance' does a lot of work.

;; This code does two things: 1) it writes a smaller persistence file,
;; and 2) it bypasses `make-instance' and tries to create objects more
;; quickly, while still providing minimal-but-sufficient type-checking
;; and safety.  This happens in the generic method
;; `ebdb-greasedpig-make-instance', which replaces `make-instance' and
;; its later calls to `initialize-instance' and `shared-initialize'.

;; If everything goes as planned, eventually
;; `ebdb-greasedpig-object-write' will be a plain `object-write', and
;; `ebdb-greasedpig-make-instance' will be renamed
;; `eieio-persistent-make-instance', and this file and its disgusting
;; prefix will go away altogether.

;; Tested with a database of 1113 records, the current results are:

;; Original persistence file: 787K
;; Greasedpig persistence file: 391K

;; Load time, with 100 iterations:
;; Original: Elapsed time: 59.960742s (35.835037s in 183 GCs)
;; Greasedpig: Elapsed time: 43.391925s (21.893583s in 120 GCs)

;; Load time, with 100 iterations and `eieio-skip-typecheck' set to t:
;; Original: Elapsed time: 46.292954s (29.979219s in 150 GCs)
;; Greasedpig: Elapsed time: 28.172554s (13.743792s in 66 GCs)

;; Bummer, was hoping for a better speedup.  And it still doesn't do
;; everything the original did.

;;; Code:

(require 'ebdb)

(defclass greasedpig ()
  ()
  "A class that works like `eieio-persistent', but faster."
  :abstract t)

(defclass ebdb-db-greasedpig (ebdb-db-file greasedpig)
  ())

;; Writing/printing

(cl-defmethod object-write ((db ebdb-db-greasedpig)
			    &optional comment)
  "Write an instance of `ebdb-db-greasedpig'."
  (when comment
    (princ comment)
    (princ "\n"))
  (ebdb-greasedpig-object-write db))

(cl-defgeneric ebdb-greasedpig-object-write ((obj eieio-default-superclass))
  "Write OBJ using a more compact list notation.
The first element of the list is the class name, followed by all
slots written in order of definition, with no initarg tag.  It
follows that all slots must be written, if only as a nil or an
`eieio-unbound'."
  (let* ((cv (cl--find-class (eieio-object-class obj)))
	 (slots (eieio--class-slots cv))
	 (slot-num (length slots))
	 slot)
    (princ (format "(%s " (eieio-object-class obj)))
    (dotimes (i slot-num)
      (setq slot (cl--slot-descriptor-name (aref slots i)))
      (if (slot-boundp obj slot)
	  ;; `eieio-oref' also does a bunch of checking, should we skip
	  ;; that?
	  (ebdb-greasedpig-object-write
	   (eieio-oref obj slot))
	(princ 'eieio-unbound))
      (unless (= i (1- slot-num))
       (princ " ")))
    (princ ")")))

(cl-defgeneric ebdb-greasedpig-object-write ((lst cons))
  ;; Something like '(#<obj> . #<not-an-obj>) will raise an error when
  ;; it reaches the mapcar.
  (if (object-p (car lst))
      (progn
	(princ "(")
	(mapcar #'ebdb-greasedpig-object-write lst)
	(princ ")"))
    (princ lst)))

(cl-defgeneric ebdb-greasedpig-object-write
    ((cls (subclass eieio-default-superclass)))
  (princ (eieio-class-name cls)))

(cl-defgeneric ebdb-greasedpig-object-write (obj)
  (princ obj))

(cl-defmethod ebdb-greasedpig-object-write :around ((obj ebdb-record))
  "Write an `ebdb-record', sans cache."
  ;; cache will cause infloop.
  (cl-call-next-method (clone obj :cache nil)))

(cl-defmethod ebdb-greasedpig-object-write :before ((obj ebdb-record))
  (princ "\n"))

;; Reading/restoring

;; 100% copied from `eieio-persistent-read'.  The idea is that
;; `eieio-persistent-convert-list-to-object' will become a class-level
;; method, and we won't have to do this.
(defun ebdb-greasedpig-restore-database (filename &optional class)
  "Read FILE and construct an EBDB greasedpig database."
  (unless class
    (message "Unsafe call to `eieio-persistent-read'."))
;  (when class (cl-check-type class class))
  (let ((ret nil)
	(buffstr nil))
    (unwind-protect
	(progn
	  (with-current-buffer (get-buffer-create " *tmp eieio read*")
	    (insert-file-contents filename nil nil nil t)
	    (goto-char (point-min))
	    (setq buffstr (buffer-string)))
	  ;; Do the read in the buffer the read was initialized from
	  ;; so that any initialize-instance calls that depend on
	  ;; the current buffer will work.
	  (setq ret (read buffstr))
	  (when (not (child-of-class-p (car ret) 'greasedpig))
	    (error "Corrupt object on disk: Unknown saved object"))
	  (when (and class
		     (not (or (eq (car ret) class) ; same class
			      (and allow-subclass
				   (child-of-class-p (car ret) class)) ; subclasses
			      )))
	    (error "Corrupt object on disk: Invalid saved class"))
	  (setq ret (ebdb-greasedpig-make-instance
		     (car ret) (cdr ret)))
	  (oset ret file filename))
      (kill-buffer " *tmp eieio read*"))
    ret))

(cl-defmethod ebdb-greasedpig-make-instance ((cls (subclass eieio-default-superclass))
					     lst)
  "Make an instance of a `greasedpig' object.
This method circumvents `make-instance', `initialize-instance',
and `shared-initialize', and assumes no slot labels were written
into the object data, and that all slots are present, if only as
nil."
  (let* ((objclass (eieio--class-object cls))
	 (object (copy-sequence (eieio--class-default-object-cache
                                 objclass)))
	 (slots (eieio-class-slots objclass)))
    ;; FIXME: This does not do the conditional eval of initforms, like
    ;; `initialize-instance' does.
    (dolist (val lst)
      (eieio-oset
       object
       (cl--slot-descriptor-name (pop slots))
       (when val ; nil is fine.
	(ebdb-greasedpig-fix-value val))))
    object))

(cl-defmethod ebdb-greasedpig-make-instance :around ((cls (subclass ebdb-record))
						     lst)
  (let ((rec (cl-call-next-method))
	(cache (make-instance 'ebdb-cache)))
    (setf (slot-value rec 'cache) cache)
    rec))

(defun ebdb-greasedpig-fix-value (val)
  (let (result)
    (when (consp val)
      (if (class-p (car val))
	  (setq result (ebdb-greasedpig-make-instance (car val)
						      (cdr val))
		val nil)
	(while (consp val)
	  (push (ebdb-greasedpig-fix-value (car val)) result)
	  (setq val (cdr val)))))
    (if (eieio-object-p result)
	result
     (nconc (nreverse result)
	    (if (stringp val)
		(substring-no-properties val)
	      val)))))

(provide 'ebdb-greasedpig)
;;; ebdb-greasedpig.el ends here
