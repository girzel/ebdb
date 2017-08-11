;;; ebdb-gnorb.el --- Utilities for connecting EBDB to Gnorb  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Version: 1
;; Package-Requires: ((gnorb "1.1.0") (ebdb "0.2"))

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
(require 'gnorb-org)
(require 'gnorb-gnus)

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

(defcustom gnorb-ebdb-collect-by-thread t
  "When collecting links to messages, only collect one link per thread.

This option won't work correctly unless `gnus-show-thread' is set
to t; if it is nil, this option will be ignored.

This also affects how links are followed: when t, following a
link will display the whole thread."

  :group 'gnorb-ebdb
  :type 'boolean)

(defcustom gnorb-ebdb-message-format "%:lapsed days: %:subject"
  "How a single message is formatted in the list of recent messages.
This format string is used in multi-line record display.

Available information for each message includes the subject, the
date, and the message's count in the list, as an integer. You can
access subject and count using the %:subject and %:count escapes.
The message date can be formatted using any of the escapes
mentioned in the docstring of `format-time-string', which see, or
the escape %:lapsed, which inserts the number of days ago the
message was received."

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

(defun gnorb-ebdb-follow-link (_record _field)
  (when-let ((link (or
		    (get-text-property (point) 'gnorb-link)
		    (get-text-property
		     (ebdb-scan-property 'gnorb-link #'gnorb-ebdb-link-p 1)
		     'gnorb-link))))
    (org-gnus-follow-link (gnorb-ebdb-link-group link)
			  (gnorb-ebdb-link-id link))
    (when (and gnus-show-threads
	       gnorb-ebdb-collect-by-thread)
      (gnus-summary-refer-thread))))

(cl-defmethod ebdb-string ((field gnorb-ebdb-field-messages))
  (format "%d messages" (length (slot-value field 'messages))))

(defun ebdb-gnorb-lapsed-days (date)
  "Return the number of days between now and DATE."
  ;; Cribbed/simplified from `article-lapsed-string'.  Need to handle
  ;; dates in the future, though that's stupid.
  (let* ((now (current-time))
	 (delta (time-subtract now date))
	 (real-sec (and delta
			(+ (* (float (car delta)) 65536)
			   (cadr delta))))
	 (sec (and delta (abs real-sec))))
    (floor (/ sec 86400))))

(cl-defmethod ebdb-fmt-field ((_fmt ebdb-formatter-ebdb)
			      (field gnorb-ebdb-field-messages)
			      _style
			      (_record ebdb-record))
  (let* ((msgs (slot-value field 'messages))
	 (outstring
	  (if (= (length msgs) 0)
	      "No message yet"
	    (mapconcat
	     #'identity
	     (let ((count 0) str)
	       (mapcar
		(lambda (m)
		  (setq str
			(format-time-string
			 (replace-regexp-in-string
			  "%:subject" (substring
				       (gnorb-ebdb-link-subject m)
				       0 (min 30
					      (length (gnorb-ebdb-link-subject m))))
			  (replace-regexp-in-string
			   "%:count" (number-to-string (cl-incf count))
			   gnorb-ebdb-message-format))
			 (gnorb-ebdb-link-date m)))
		  ;; Avoid doing the lapse calculation if not
		  ;; necessary.  Of course, this is probably more
		  ;; wasteful than just doing it anyway.
		  (when (string-match-p "%:lapsed" str)
		    (setq str
			  (replace-regexp-in-string
			   "%:lapsed" (number-to-string
				       (ebdb-gnorb-lapsed-days
					(gnorb-ebdb-link-date m)))
			   str)))
		  (propertize
		   str
		   'face 'gnorb-ebdb-link
		   'gnorb-link m))
		msgs))
	     "\n"))))
    outstring))

(cl-defmethod ebdb-notice-field ((field gnorb-ebdb-field-messages)
				 (_type (eql sender))
				 (record ebdb-record))
  "Used in the `bbdb-notice' to possibly save a link
to a message into the record's `gnorb-ebdb-messages-field'."

  (with-current-buffer gnus-summary-buffer
    (let* ((links (slot-value field 'messages))
	   (art-no (gnus-summary-article-number))
	   (heads (gnus-summary-article-header art-no))
	   (date (apply 'encode-time
			(parse-time-string (mail-header-date heads))))
	   (refs (gnus-extract-references (mail-header-references heads)))
	   (subject (gnus-simplify-subject (mail-header-subject heads)))
	   (id (mail-header-id heads))
	   (group (gnorb-get-real-group-name
		   gnus-newsgroup-name
		   art-no))
	   link)
      (if (not (and date subject id group))
	  (message "Could not save a link to this message")
	(setq link (make-gnorb-ebdb-link :subject subject :date date
					 :group group :id id))
	(when (and gnus-show-threads
		   gnorb-ebdb-collect-by-thread)
	  ;; If the new link has a ref to an earlier link, then don't
	  ;; save the new link, but do update the date of the earlier
	  ;; link. Ie, the new link isn't kept, but it "refreshes" the
	  ;; date of the whole thread.
	  (dolist (l links)
	    (when (member (gnorb-ebdb-link-id l)
			  refs)
	      (setf (gnorb-ebdb-link-date l) date)
	      ;; We can discard link.
	      (setq link nil))))
	(when link
	  (setq links (cons link (delete link links))))
	(when (eq gnorb-ebdb-define-recent 'received)
	  (setq links (sort links
			    (lambda (a b)
			      (time-less-p
			       (gnorb-ebdb-link-date b)
			       (gnorb-ebdb-link-date a))))))
	(setq links (cl-subseq links 0 (min (length links)
					    gnorb-ebdb-collect-N-messages)))
	(ebdb-record-change-field
	 record field
	 (make-instance 'gnorb-ebdb-field-messages
			:messages links))))))

(eieio-defclass-autoload
 'gnorb-ebdb-field-messages
 'ebdb-field-user
 "ebdb-gnorb"
 "Gnorb field holding links to Gnus messages.")

(provide 'ebdb-gnorb)
