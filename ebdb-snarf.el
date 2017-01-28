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
  (let ((name-re "\\(?:From: \\|To: \\|Cc: \\)?\\(\\(?:[[:upper:]][[:lower:]]+[,[:space:]]*\\)\\{1,\\}\\)")
	(mail-re "\\([^[:space:]:<[]+@[^]:[:space:]>]+\\)")
	bundles
	(case-fold-search nil))

    ;; The structure we'll return is a list of lists.  Each element is
    ;; a list of objects (records and fields) that we believe are
    ;; associated.  If RECORDS is given, then we have something to
    ;; start with.

    (when records
      (setq bundles (mapcar (lambda (r)
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
    ;; words, which is dumb.  Also, the code only scans for mail
    ;; addresses at the moment.

    (goto-char (point-min))

    (while (re-search-forward mail-re nil t)
      (let* ((mail (match-string-no-properties 1))
	     (sticker (line-end-position))
	     (name (save-excursion
		     (goto-char (progn (line-beginning-position)))
		     (when (re-search-forward name-re sticker t)
		       (string-trim (match-string-no-properties 1)))))
	     ;; Make a regular expression that stands a chance of
	     ;; matching an existing record.
	     (mail-user-re
	      (regexp-opt
	       (append (split-string
			(downcase (car (split-string mail "@")))
			"[-_.]" t)
		       (when name
			 (split-string
			  (downcase name)
			  "[, ]" t)))))
	     group)
	;; See if any of this information fits what we've got in
	;; RECORDS.
	(unless (catch 'match
		  (dolist (elt bundles)
		    (let ((r (plist-get elt :record))
			  (n (plist-get elt :names))
			  (m (plist-get elt :mail))
			  (case-fold-search t))
		      (when (cond
			     ((and r
				   (ebdb-search (list r)
						`((name ,mail-user-re)
						  (mail ,mail-user-re)))))
			     ((and m
				   (ebdb-field-search m mail-user-re)))
			     ((and n
				   (seq-find
				    (lambda (na)
				      (ebdb-field-search na mail-user-re))
				    n)))
			     (t nil))
			(unless (and m	; mail is already in here.
				     (string-match-p (ebdb-string m) mail))
			  (plist-put
			   elt
			   :mail (ebdb-parse ebdb-default-mail-class mail)))
			(when (and name
				   (or (null n)
				       (null (seq-find
					      (lambda (na)
						(string-match-p
						 (ebdb-string na)
						 name))
					      n))))
			  (plist-put
			   elt
			   :names (append (list (ebdb-parse ebdb-field-name name))
					  n)))
			(throw 'match t)))))
	  (setq group
		(plist-put group :mail (ebdb-parse ebdb-default-mail-class mail)))
	  (when name
	    (setq group (plist-put
			 group
			 :names (list (ebdb-parse ebdb-field-name name)))))
	  (push group bundles))))
    bundles))

(defun ebdb-snarf-process (input)
  "Process INPUT, which is a list of bundled information
  representing records.

Either find the matching records, update existing (but
incomplete) records, or create new records.  Then display them."
  (let (record names name-alist name mail phone address slot records)
    (dolist (bundle input)
      (setq record (plist-get bundle :record)
	    names (plist-get bundle :names)
	    mail (plist-get bundle :mail)
	    phone (plist-get bundle :phone)
	    address (plist-get bundle :address))
      ;; Either we were given a record...
      (unless record
	;; ...we can find one in the database...
	(unless (setq record (car-safe
			      (ebdb-message-search
			       (and names (regexp-opt (mapcar #'ebdb-string names)))
			       (and mail (ebdb-string mail)))))
	  ;; ...or we create a new one.
	  (when (yes-or-no-p
		 (format "Create new record%s? "
			 (cond (names
				(format " for %s"
					(mapconcat #'ebdb-string
						   names "/")))
			       (mail
				(format " for %s" (ebdb-string mail)))
			       (t ""))))
	    (setq record
		  (make-instance
		   ebdb-default-record-class
		   :name (progn
			   (setq name
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
				       (t nil)))
			   (ebdb-read ebdb-default-name-class nil name))))
	    (ebdb-db-add-record (car ebdb-db-list) record)
	    (ebdb-init-record record)
	    (when mail
	      (ebdb-record-insert-field record 'mail mail)))))
      (dolist (elt (delq nil (list phone address)))
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
	   elt)))
      (push record records))
    (when records
      (ebdb-display-records records nil nil t (ebdb-popup-window)
			    (format "*%s-Snarf*" ebdb-buffer-name)))))

(provide 'ebdb-snarf)
;;; ebdb-snarf.el ends here
