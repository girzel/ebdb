;;; ebdb-gnorb.el --- Utilities for connecting EBDB to Gnorb  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Free Software Foundation, Inc.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bits and pieces useful for tying EBDB in with Gnorb.  Everything in
;; this file can be moved elsewhere.

;;; Code:

(require 'ebdb-format)
(require 'ebdb-com)

;; Probably we shouldn't use this, EBDB should not have a dependency
;; on Org.

;; Actually the real problem is that this is Gnus specific, we should
;; have equivalent methods for all the MUAs, something like
;; `ebdb-mua-find-messages'.
(autoload 'org-gnus-follow-link "org-gnus")
(autoload 'article-lapsed-string "gnus-art")

(defgroup gnorb-ebdb nil
  "Customizations for Gnorb-specific functionality."
  :group 'ebdb)

(defcustom gnorb-ebdb-collect-N-messages 5
  "For records with a `ebdb-gnorb-messages-field',
collect links to a maximum of this many messages."

  :group 'gnorb-ebdb
  :type 'integer)

(defcustom gnorb-ebdb-define-recent 'seen
  "For records with a `gnorb-ebdb-message-tag-field',
this variable controls how gnorb defines a \"recent\" message.
Setting it to the symbol 'seen will collect the messages most
recently opened and viewed. The symbol 'received means gnorb will
collect the most recent messages by Date header.

In other words, if this variable is set to `received', and a
record's messages field is already full of recently-received
messages, opening a five-year-old message (for instance) from
this record will not push a link to the message into the field."

  :group 'gnorb-ebdb
  :type '(choice (const :tag "Most recently seen" 'seen)
                 (const :tag "Most recently received" 'received)))

(defcustom gnorb-ebdb-message-format "%:count. %:lapsed: %:subject"
  "How a single message is formatted in the list of recent messages.
This format string is used in multi-line record display.

Available information for each message includes the subject, the
date, and the message's count in the list, as an integer. You can
access subject and count using the %:subject and %:count escapes.
The message date can be formatted using any of the escapes
mentioned in the docstring of `format-time-string', which see, or
the escape %:lapsed, which shows how many days ago the message
was received."

  :group 'gnorb-ebdb
  :type 'string)

(defface gnorb-ebdb-link '((t :inherit org-link))
  "Custom face for displaying message links in the *BBDB* buffer.
  Defaults to org-link."
  :group 'gnorb-ebdb)

(cl-defstruct gnorb-ebdb-link
  subject date group id)

(defclass gnorb-ebdb-field-messages (ebdb-field-user)
  ((messages
    :type (list-of gnorb-ebdb-link)
    :initarg :messages
    :initform nil)
   (actions :initform '(("Follow link" . gnorb-ebdb-follow-link))))
  :human-readable "gnus messages")

(defun gnorb-ebdb-follow-link (record field)
  (when-let ((link (or
		    (get-text-property (point) 'gnorb-link)
		    (get-text-property
		     (ebdb-scan-property 'gnorb-link #'gnorb-ebdb-link-p 1)
		     'gnorb-link))))
    (org-gnus-follow-link (gnorb-ebdb-link-group link)
			  (gnorb-ebdb-link-id link))))

(cl-defmethod ebdb-string ((field gnorb-ebdb-field-messages))
  (format "%d messages" (length (slot-value field 'messages))))

(cl-defmethod ebdb-fmt-field ((fmt ebdb-formatter-ebdb)
			      (field gnorb-ebdb-field-messages)
			      _style
			      (record ebdb-record))
  ;; Hard-code this for now, we can provide more options later.
  (let* ((article-time-units '((day . 86400)))
	 (msgs (slot-value field 'messages))
	 (outstring
	  (if (= (length msgs) 0)
	      "No message yet"
	    (mapconcat
	     #'identity
	     (let ((count 0))
	       (mapcar
		(lambda (m)
		  (propertize
		   (format-time-string
		    (replace-regexp-in-string
		     "%:lapsed" (article-lapsed-string
				 (gnorb-ebdb-link-date m) 1)
		     (replace-regexp-in-string
		      "%:subject" (substring
				   (gnorb-ebdb-link-subject m)
				   0 (min 30
					  (length (gnorb-ebdb-link-subject m))))
		      (replace-regexp-in-string
		       "%:count" (number-to-string (cl-incf count))
		       gnorb-ebdb-message-format)))
		    (gnorb-ebdb-link-date m))
		   'face 'gnorb-ebdb-link
		   'gnorb-link m))
		msgs))
	     "\n"))))
    outstring))

(cl-defmethod ebdb-notice-field ((field gnorb-ebdb-field-messages)
				 (_type (eql from))
				 _hdrs
				 (record ebdb-record))
  "Used in the `bbdb-notice' to possibly save a link
to a message into the record's `gnorb-ebdb-messages-field'."

  (when (memq major-mode '(gnus-summary-mode gnus-article-mode))
    (with-current-buffer gnus-summary-buffer
      (let* ((val (slot-value field 'messages))
	     (art-no (gnus-summary-article-number))
	     (heads (gnus-summary-article-header art-no))
	     (date (apply 'encode-time
			  (parse-time-string (mail-header-date heads))))
	     (subject (mail-header-subject heads))
	     (id (mail-header-id heads))
	     (group (gnorb-get-real-group-name
		     gnus-newsgroup-name
		     art-no))
	     link)
	(if (not (and date subject id group))
	    (message "Could not save a link to this message")
	  (setq link (make-gnorb-ebdb-link :subject subject :date date
					   :group group :id id))
	  (setq val (cons link (delete link val)))
	  (when (eq gnorb-ebdb-define-recent 'received)
	    (setq val (sort val
			    (lambda (a b)
			      (time-less-p
			       (gnorb-bbdb-link-date b)
			       (gnorb-bbdb-link-date a))))))
	  (setq val (cl-subseq val 0 (min (length val) gnorb-ebdb-collect-N-messages)))
	  (ebdb-record-change-field
	   record field
	   (make-instance 'gnorb-ebdb-field-messages
			  :messages val)))))))

(provide 'ebdb-gnorb)
