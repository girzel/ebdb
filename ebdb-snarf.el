;;; ebdb-snarf.el --- Creating or displaying records based on free-form pieces of text  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Eric Abrahamsen

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

;;; Code:

(require 'ebdb)

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
  (let ((name-re "\\(\\(?:[[:upper:]][[:lower:]]+[[:blank:]]?\\)+\\)")
	(mail-re "\\([^[:space:]]+@[^[:space:]>]+\\)")
	(case-fold-search nil)
	group name mail phone address sticker)

    ;; The structure we'll return is a list of lists.  Each element is
    ;; a list of objects (records and fields) that we believe are
    ;; associated.  If RECORDS is given, then we have something to
    ;; start with.

    (when records
      (setq records (mapcar (lambda (r)
			      (list :record r))
			    records)))

    ;; We don't explicitly search for names, because how would you
    ;; know?  Instead, we look for things that appear to be names,
    ;; that come right before some other field information.  Ie:

    ;; John Bob <john@bob.com>

    ;; John Bob (555) 555-5555

    ;; John Bob
    ;; 1111 Upsidedown Drive
    ;; Nowhere, Massachusetts, 55555

    ;; Currently the definition of "name" is a series of capitalized
    ;; words, which is dumb.

    (goto-char (point-min))

    ;; Scan for mail addresses.  This is all we're going to do, until
    ;; the basics are in place.
    (while (re-search-forward mail-re nil t)
      (setq mail (match-string-no-properties 1)
	    sticker (line-end-position)
	    name (save-excursion
		   (goto-char (progn (forward-line -1)
				     (line-beginning-position)))
		   ;; This allows for a name before the email address,
		   ;; or on the line above it.  Probably this is too
		   ;; permissive.
		   (when (re-search-forward name-re sticker t)
		     (match-string-no-properties 1))))
      ;; See if any of this information fits what we've got in
      ;; RECORDS.
      (let* ((case-fold-search nil)
	     (name (when name (string-trim name)))
	     (mail-user
	      (replace-regexp-in-string "[.-]" ""
					(car (split-string mail "@")))))
	(unless (catch 'match
		  (when name
		    (setq mail-user (regexp-opt
				     (list mail-user
					   name))))
		  (dolist (elt records)
		    (dolist (thing elt)
		      (when (cond
			     ((and (object-of-class-p thing ebdb-record)
				   (or (ebdb-search (list thing)
						    `((name ,mail-user)
						      (mail ,mail-user))))))
			     ((and (object-of-class-p thing ebdb-field-name)
				   (ebdb-field-search thing mail-user)))
			     (t nil))
			(plist-put elt :mail (ebdb-parse ebdb-default-mail-class mail))
			(when name
			  (plist-put elt :name (ebdb-parse ebdb-field-name name)))
			(throw 'match t)))))
	  (setq group
		(plist-put group :mail (ebdb-parse ebdb-default-mail-class mail)))
	  (when name
	    (setq group (plist-put group :name (ebdb-parse ebdb-field-name name))))
	  (push group records))))
    records))

(defun ebdb-snarf-process (input)
  "Process INPUT, which is a list of bundled information
  representing records.

Either find the matching records, update existing (but
incomplete) records, or create new records.  Then display them."
  (let (record name mail phone address slot records)
    (dolist (bundle input)
      (setq record (plist-get bundle :record)
	    name (plist-get bundle :name)
	    mail (plist-get bundle :mail)
	    phone (plist-get bundle :phone)
	    address (plist-get bundle :address))
      (unless record
	(unless (setq record (car-safe
			      (ebdb-message-search
			       (and name (ebdb-string name))
			       (and mail (ebdb-string mail)))))
	  (when (yes-or-no-p
		 (format "Create new record%s? "
			 (if name
			     (format " for %s" (ebdb-string name))
			   "")))
	    (setq record (make-instance ebdb-default-record-class
					:name (or name (ebdb-read ebdb-default-name-class))))
	    (ebdb-db-add-record (car ebdb-db-list) record)
	    (ebdb-init-record record))))
      (dolist (elt (list mail phone address))
	(when elt
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
	     elt))))
      (push record records))
    (when records
      (ebdb-display-records records nil nil t (ebdb-popup-window)
			    (format "*%s-Snarf*" ebdb-buffer-name)))))

(provide 'ebdb-snarf)
;;; ebdb-snarf.el ends here
