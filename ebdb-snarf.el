;;; ebdb-snarf.el --- Creating or displaying records based on free-form pieces of text  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions for reading arbitrary bits of text,
;; interpreting them as records and fields, and then using them to
;; search/display/update/create records.

;; The main entry point is the interactive command `ebdb-snarf'.  It
;; figures out what text we're dealing with, puts the text in a temp
;; buffer, calls `ebdb-snarf-collect', and passes the results of that
;; to `ebdb-snarf-process'.

;; `ebdb-snarf-collect' is responsible for collecting everything in
;; the buffer that looks like information that could represent a
;; record, or a field.  It creates actual field instances if possible,
;; and puts them into likely groups.  `ebdb-snarf-process' is
;; responsible for prompting the user to actually create or update
;; records, and for displaying the results.

;; Right now all the code does is search for mail addresses.  A more
;; generalized version might involve a defcustom called
;; `ebdb-snarf-routines'.  This should be an alist whose elements look
;; like (ebdb-field-class "regexp one" "regexp two" ...).  Or,
;; construct this list from other field-class-specific lists.

;; Anyway, the idea is that we blindly scan the snarf buffer for the
;; regexps in that list, construct the appropriate record or field
;; instances, group them using calls to `ebdb-string' and
;; `ebdb-field-search'/`ebdb-search'.  With a little fancy footwork it
;; should be possible to keep the code mostly field agnostic.

;;; Code:

(require 'ebdb)

(defgroup ebdb-snarf nil
  "Options for EBDB snarfing."
  :group 'ebdb)

(defcustom ebdb-snarf-routines
  '((ebdb-field-mail "\\([^[:space:]:<[]+@[^]:[:space:]>]+\\)"))

  "An alist of EBDB field classes and related regexps.

Each alist element is an EBDB field class symbol, followed by a
list of regular expressions that can be used to produce instances
of that class when passed to `ebdb-parse'."

  :group 'ebdb-snarf
  :type 'list)

(defcustom ebdb-snarf-name-re
  (list "\\(?:[[:upper:]][[:lower:]]+[,[:space:]]*\\)\\{1,\\}")

  "A list of regular expressions matching names.

This is a separate option from `ebdb-snarf-routines' because
snarfing doesn't search for names separately, only in conjunction
with other field types."

  :group 'ebdb-snarf
  :type 'list)

;;;###autoload
(defun ebdb-snarf (&optional string start end)
  "Snarf text and attempt to display/update/create a record from it.

If STRING is given, snarf the string.  If START and END are given
in addition to STRING, assume they are 0-based indices into it.
If STRING is nil but START and END are given, assume they are
buffer positions, and snarf the region between.  If all three
arguments are nil, snarf the entire current buffer."
  (interactive
   (list nil
	 (region-beginning)
	 (region-end)))
  (let (str)
    (cond ((and (or start end) string)
	   (setq str (substring string start end)))
	  ((and start end (null string))
	   (setq str (buffer-substring-no-properties start end)))
	  (string
	   (setq str string))
	  (t
	   (setq str (buffer-string))))
    (with-temp-buffer
      (insert (string-trim str))
      (ebdb-snarf-process (ebdb-snarf-collect)))))

(defun ebdb-snarf-collect (&optional records)
  "Collect EBDB record information from the text of the current buffer.

This function will find everything that looks like field
information, and do its best to either associate that information
with existing records, or to organize it into likely groups.  If
RECORDS is given, it should be a list of records that we think
have something to do with the text in the buffer."
  (let ((case-fold-search nil)
	bundles)

    ;; The structure we'll return is a list of lists.  Each element is
    ;; a list of objects (records and fields) that we believe are
    ;; associated.  If RECORDS is given, then we have something to
    ;; start with.

    (when records
      (setq bundles (mapcar (lambda (r)
			      (list r))
			    records)))

    ;; We don't explicitly search for names, because how would you
    ;; know?  Instead, we look for things that appear to be names,
    ;; that come right before some other field information.  Eg:

    ;; John Bob <john@bob.com>

    ;; John Bob (555) 555-5555

    ;; John Bob
    ;; 1111 Upsidedown Drive
    ;; Nowhere, Massachusetts, 55555

    ;; For each individual regular expression, we scan the whole
    ;; buffer and create single field-class instances from any
    ;; matches, and possibly an accompanying name-class instance, and
    ;; decide what to do about both of them.
    (dolist (class ebdb-snarf-routines)
      (dolist (re (cdr class))
	(goto-char (point-min))
	(while (re-search-forward re nil t)
	  (condition-case nil
	      (let* ((found (ebdb-parse
			     (car class)
			     (match-string-no-properties 1)))
		     (sticker (line-end-position))
		     (name (save-excursion
			     (goto-char (progn (line-beginning-position)))
			     (when (re-search-forward
				    (concat
				     ;; When snarfing messages, we're
				     ;; likely to see email headers in the
				     ;; message body.
				     "\\(?:From: \\|To: \\|Cc: \\)?"
				     (regexp-opt ebdb-snarf-name-re))
				    sticker t)
			       ;; If something goes wrong with the
			       ;; name, don't worry about it.
			       (ignore-errors
				 (ebdb-parse
				  ebdb-field-name
				  (string-trim (match-string-no-properties 1)))))))
		     ;; Make a regular expression that stands a chance
		     ;; of matching an existing record or record
		     ;; fields.
		     (generic-re
		      (regexp-opt
		       (append (split-string
				(downcase (ebdb-string found))
				"[-_.@]" t)
			       (when name
				 (split-string
				  (downcase (ebdb-string name))
				  "[, ]" t)))))
		     group)
		;; See if any of this information fits what we've got in
		;; RECORDS.
		(unless (catch 'match
			  (dolist (b bundles)
			    (dolist (elt b)
			      (when (cond
				     ((and (object-of-class-p elt ebdb-record)
					   (ebdb-search (list elt)
							`((name ,generic-re)
							  (mail ,generic-re)))))
				     ((and (object-of-class-p elt ebdb-field)
					   (ebdb-field-search elt generic-re)))
				     (t nil))
				(unless (assoc-string
					 (ebdb-string found)
					 (mapcar #'ebdb-string b))
				  (push found b))
				(when (and name
					   (null (assoc-string
						  (ebdb-string name)
						  (mapcar #'ebdb-string b))))
				  (push name b))
				(throw 'match t)))))
		  ;; If it doesn't, make a new grouping.
		  (push found group)
		  (when name
		    (push name group))
		  (push group bundles)))
	    ;; If a regular expression matches but the result is
	    ;; unparseable, that means the regexp is bad and should be
	    ;; changed.  Later, report these errors if `ebdb-debug' is
	    ;; true.
	    (ebdb-unparseable nil)))))
    bundles))

(defun ebdb-snarf-process (input)
  "Process INPUT, which is a list of bundled information
  representing records.

Either find the matching records, update existing (but
incomplete) records, or create new records.  Then display them."
  (dolist (bundle input)
    (let (record names name-alist mails fields records)
      ;; Record instances, name-class instances, and mail-class
      ;; instances need to be treated specially.
      (dolist (elt bundle)
	(cond ((object-of-class-p elt ebdb-record)
	       (setq record elt))
	      ((object-of-class-p elt ebdb-field-name)
	       (push elt names))
	      ((object-of-class-p elt ebdb-field-mail)
	       (push elt mails))
	      ((object-of-class-p elt ebdb-field)
	       (push elt fields))
	      (t nil)))
      ;; Either we were given a record...
      (unless record
	;; ...we can find one in the database...
	(unless (setq record (car-safe
			      (ebdb-search
			       (ebdb-records)
			       (append
				(when names
				  `(name ,(regexp-opt
					   (mapcar #'ebdb-string names))))
				(when mails
				  `(mail ,(regexp-opt
					   (mapcar #'ebdb-string mails))))))))
	  ;; ...or we create a new one.
	  (when (yes-or-no-p
		 (format "Create new record%s? "
			 (if (or names mails)
			     (format " for %s"
				     (mapconcat #'ebdb-string
						(append names mails)
						"/"))
			   "")))
	    (setq record
		  (make-instance
		   ebdb-default-record-class
		   :name (ebdb-read
			  ebdb-default-name-class nil
			  (progn
			    ;; I hate completing read.
			    (cond ((= 1 (length names))
				   (car names))
				  ((setq name-alist
					 (mapcar (lambda (n)
						   (cons (ebdb-string n)
							 n))
						 names))
				   (cdr
				    (assoc-string
				     (completing-read
				      "Use name: "
				      name-alist)
				     name-alist)))
				  (t nil))))))
	    (ebdb-db-add-record (car ebdb-db-list) record)
	    (ebdb-init-record record)))))
    (dolist (elt (append mails fields))
      ;; What if user said no to creating a new record above?
      (setq slot (car (ebdb-record-field-slot-query
		       (eieio-object-class record)
		       `(nil . ,(eieio-object-class elt)))))
      (when (and slot
		 (null (or (equal elt (slot-value record slot))
			   (member elt (slot-value record slot))))
		 (yes-or-no-p (format "Add %s to %s? "
				      (ebdb-string elt)
				      (ebdb-string record))))
	(ebdb-record-insert-field
	 record
	 slot
	 elt)
	(ebdb-init-field elt record)))
    (push record records))
  (when records
    (ebdb-display-records records nil nil t (ebdb-popup-window)
			  (format "*%s-Snarf*" ebdb-buffer-name))))

(provide 'ebdb-snarf)
;;; ebdb-snarf.el ends here
