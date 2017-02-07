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
;; buffer, and calls three nested functions: `ebdb-snarf-collect',
;; which finds likely field-related strings in the buffer and groups
;; them, then `ebdb-snarf-collapse', which tries to match that
;; information to existing records, and finally `ebdb-snarf-query',
;; which queries the user about how to handle leftover
;; information. Any resulting records are then displayed.

;; The option `ebdb-snarf-routines' contains regexps that can be used
;; to construct field instances.  `ebdb-snarf-collect' uses the
;; elements of this list to search for relevant strings.

;;; Code:

(require 'ebdb)

(defgroup ebdb-snarf nil
  "Options for EBDB snarfing."
  :group 'ebdb)

(defcustom ebdb-snarf-routines
  '((ebdb-field-mail "\\([^[:space:]\":\n<[]+@[^]:[:space:]>\"\n]+\\)"))

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
  (interactive)
  (let ((str
	 (cond ((use-region-p)
		(buffer-substring-no-properties
		 (region-beginning) (region-end)))
	       ((and (or start end) string)
		(substring string start end))
	       ((and start end (null string))
		(buffer-substring-no-properties start end))
	       (string
		string)
	       (t
		(buffer-string))))
	records)
    (with-temp-buffer
      (insert (string-trim str))
      (setq records (ebdb-snarf-query
		     (ebdb-snarf-collapse
		      (ebdb-snarf-collect)))))
    (when records
      (ebdb-display-records records nil nil t (ebdb-popup-window)
			    (format "*%s-Snarf*" ebdb-buffer-name)))))

(defun ebdb-snarf-collect (&optional records)
  "Collect EBDB record information from the text of the current buffer.

This function will find everything that looks like field
information, and do its best to organize it into likely groups.
If RECORDS is given, it should be a list of records that we think
have something to do with the text in the buffer."
  (let ((case-fold-search nil)
	bundles)

    ;; The structure we'll return is a list of vectors, containing
    ;; records and fields we believe are associated.  Each vector has
    ;; three elements: a record, a list of name instances, and a list
    ;; of all other fields.  If RECORDS is given, then we have
    ;; something to start with.

    (when records
      (setq bundles (mapcar (lambda (r)
			      (vector r nil nil))
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
		     (name (save-excursion
			     (goto-char (progn (when (= (point-at-bol)
							(match-beginning 0))
						 (forward-line -1))
					       (line-beginning-position)))
			     (when (re-search-forward
				    (concat
				     ;; When snarfing messages, we're
				     ;; likely to see email headers in
				     ;; the message body, for instance
				     ;; in quoted replies.
				     "\\(?:From: \\|To: \\|Cc: \\)?"
				     (mapconcat #'identity
						ebdb-snarf-name-re "\\|"))
				    (match-beginning 0) t)
			       ;; If something goes wrong with the
			       ;; name, don't worry about it.
			       (ignore-errors
				 (ebdb-parse
				  'ebdb-field-name
				  (string-trim (match-string-no-properties 0)))))))
		     ;; Make a regular expression that stands a chance
		     ;; of matching an existing record or record
		     ;; fields.  This is likely *too* permissive.
		     (generic-re
		      (regexp-opt
		       (append (split-string
				(downcase (ebdb-string found))
				"[-_.@)(]" t)
			       (when name
				 (split-string
				  (downcase (ebdb-string name))
				  "[, ]" t))))))
		;; See if any of this information fits what we've got in
		;; BUNDLES.
		(unless (catch 'match
			  (dolist (b bundles)
			    ;; Can't directly use `pcase-dolist'
			    ;; because the bound variables are not
			    ;; generalized variables: you can't assign
			    ;; to them.  It would be nice to have a
			    ;; `pcase-letf'!
			    (pcase-let ((`[,record ,names ,fields] b))
			      (when (or (and record
					     (ebdb-search (list record)
							  `((ebdb-field-name ,generic-re)
							    (,(car class) ,generic-re))))
					(and (or fields names)
					     (seq-some
					      (lambda (elt)
						(ebdb-field-search elt generic-re))
					      (append fields names))))
				;; It seems to match, check if the field
				;; or name are already in the bundle.
				(unless (and fields
					     (assoc-string
					      (ebdb-string found)
					      (mapcar #'ebdb-string fields)))
				  (push found (aref b 2)))
				(unless (or (null name)
					    (and names
						 (null (assoc-string
							(ebdb-string name)
							(mapcar #'ebdb-string names)))))
				  (push name (aref b 1)))
				(throw 'match t)))))
		  ;; If it doesn't, add a new grouping to BUNDLES.
		  (push (vector nil (when name (list name)) (list found))
			bundles)))
	    ;; If a regular expression matches but the result is
	    ;; unparseable, that means the regexp is bad and should be
	    ;; changed.  Later, report these errors if `ebdb-debug' is
	    ;; true.
	    (ebdb-unparseable nil)))))
    bundles))

(defun ebdb-snarf-collapse (input)
  "Process INPUT, which is a list of bundled field information.

INPUT is probably produced by `ebdb-snarf-collect'.  It should be
a list of vectors, each with three elements: a single record, a
list of name field instances, and a list of other field
instances.  Any of the three elements can be nil.

Compare each bundle against the database, and where possible find
existing records that match information in the bundle.  Discard
redundant fields, or fields that are incompatible with the record
they're grouped with.  Return the same list of (possibly altered)
vectors, usually to `ebdb-snarf-query'."
  (let (output)
    (pcase-dolist (`[,record ,names ,fields] input)
      (let (out-fields out-names)
	(unless record
	  (if-let ((rec (car-safe
			 (ebdb-search
			  (ebdb-records)
			  (mapcar
			   (lambda (f)
			     (list (eieio-object-class-name f)
				   (ebdb-string f)))
			   (append fields names))))))
	      (setq record rec)))
	(if record
	    (let (slot)
	      (dolist (f fields)
		(condition-case nil
		    (progn
		      (setq slot (car (ebdb-record-field-slot-query
				       (eieio-object-class record)
				       `(nil . ,(eieio-object-class f)))))
		      ;; Make sure that record can accept field, and doesn't
		      ;; already have it.
		      (unless (if-let ((slot-val (ignore-errors
						   (ebdb-record-field record slot))))
				  (member (ebdb-string f)
					  (mapcar #'ebdb-string
						  (if (listp slot-val)
						      slot-val
						    (list slot-val)))))
			(push f out-fields)))
		  (ebdb-unacceptable-field nil)))
	      (dolist (name names)
		(unless (ebdb-record-search
			 record 'ebdb-field-name (ebdb-string name))
		  (push name out-names))))
	  (setq out-names names
		out-fields fields))
	(push (vector record out-names out-fields) output)))
    output))

(defun ebdb-snarf-query (input)
  "Query the user about INPUT, which is a list of vectors of
  bundled information representing records.

Ask about field instances that we haven't been able to handle
automatically."
  (let (leftovers records)
    (pcase-dolist (`[,record ,names ,fields] input)
      (unless record
	;; There's no record, query-create a new one.
	(when (yes-or-no-p
	       (format "Create new record%s? "
		       (if (or fields names)
			   (format " for %s"
				   (mapconcat #'ebdb-string
					      (append fields names)
					      "/"))
			 "")))
	  ;; Which name do we use?
	  (let* ((name-alist
		  (when names
		    (mapcar (lambda (n)
			      (cons (ebdb-string n)
				    n))
			    names)))
		 (name
		  ;; I hate completing read.
		  (cond ((= 1 (length name-alist))
			 (cdar name-alist))
			(name-alist
			 (cdr
			  (assoc-string
			   (completing-read
			    "Use name: "
			    name-alist)
			   name-alist)))
			(t nil))))
	    (setq record
		  (make-instance
		   ebdb-default-record-class
		   :name (ebdb-read
			  ebdb-default-name-class nil
			  name)))
	    (when name
	      (setq names (delq name names)))
	    (ebdb-db-add-record (car ebdb-db-list) record)
	    (ebdb-init-record record))))
      (if record
	  ;; We have a record, which of the fields and names should we
	  ;; add to it?
	  (progn (dolist (elt fields)
		   (if (yes-or-no-p (format "Add %s to %s? "
					    (ebdb-string elt)
					    (ebdb-string record)))
		       (condition-case nil
			   (let ((slot (car (ebdb-record-field-slot-query
					     (eieio-object-class record)
					     `(nil . ,(eieio-object-class elt))))))
			     (ebdb-record-insert-field
			      record
			      slot
			      elt)
			     (ebdb-init-field elt record))
			 (ebdb-unacceptable-field nil))
		     (push elt leftovers)))
		 (dolist (n names)
		   (if (yes-or-no-p (format "Add %s as an aka for %s? "
					    (ebdb-string n)
					    (ebdb-string record)))
		       (progn (ebdb-record-insert-field
			       record 'aka name)
			      (ebdb-init-field name record))
		     (push name leftovers))))
	;; We have no record, dump all the fields into LEFTOVERS.
	(setq leftovers (append fields names leftovers)
	      fields nil
	      names nil))
      (when record
	(push record records)))
    ;; Handle fields in LEFTOVERS.
    (dolist (f leftovers)
      (when-let ((record
		  (cond ((yes-or-no-p
			  (format "Add %s to existing record? "
				  (ebdb-string (cdr f))))
			 (ebdb-prompt-for-record))
			((yes-or-no-p
			  (format "Add %s to new record? "
				  (ebdb-string (cdr f))))
			 (ebdb-init-record
			  (ebdb-db-add-record
			   (car ebdb-db-list)
			   (ebdb-read ebdb-default-record-class))))
			(t nil))))
	(condition-case nil
	    (let ((slot (car (ebdb-record-field-slot-query
			      (eieio-object-class record)
			      `(nil . ,(eieio-object-class f))))))
	      (ebdb-record-insert-field record slot f)
	      (ebdb-init-field f record)
	      (add-to-list records record))
	  (ebdb-unacceptable-field nil))))
    records))

(provide 'ebdb-snarf)
;;; ebdb-snarf.el ends here
