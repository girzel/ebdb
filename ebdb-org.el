;;; ebdb-org.el --- Org mode integration for EBDB    -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2017  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: 

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

;; Org mode integration for EBDB.  At present this just defines a link
;; type; at some point we'll reproduce the Agenda anniversary
;; mechanisms from org-bbdb.el.

;; EBDB links can come in several varieties.  A plain string is
;; matched against record names in the database.  Otherwise, the
;; string can be prefixed with a field type, to search only on those
;; field values.  The prefix is separated with a forward slash.
;; Examples:

;; 1. "ebdb:uuid/af1373d6-4ba1-46a7-aa4b-845db01bc2ab" (link to unique
;; record)

;; 2. "ebdb:mail/google.com" (all records with google.com email
;; addresses).  These field name "shorthands" include "uuid", "mail",
;; "phone", "address", "notes", and "tags" (this last for the
;; `ebdb-org-field-tags' class defined in this file).

;; 3. "ebdb:ebdb-field-foobar/baz" (search on a particular field
;; class)

;; Valid prefixes include all the values accepted by
;; `ebdb-record-field', as well as the names of field classes.

;; When calling `org-store-link' on a contact, a "ebdb:uuid/" style
;; link is created by default.

;; This file also defines a "tags" field class, for tagging EBDB
;; contacts with Org tags.

;;; Code:

(require 'ebdb)

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "ebdb"
			     :follow 'ebdb-org-open
			     :complete (lambda ()
					 (format
					  "ebdb:uuid/%s"
					  (ebdb-record-uuid (ebdb-prompt-for-record (ebdb-records)))))
			     :store 'ebdb-org-store-link
			     :export 'ebdb-org-export)
  (with-no-warnings ;; I know it's obsolete.
   (org-add-link-type "ebdb" #'ebdb-org-open #'ebdb-org-export)
   (add-hook 'org-store-link-functions 'ebdb-org-store-link)))

;; TODO: Put a custom keymap on the links (or else expand
;; `ebdb-org-open') so that users can choose what to do with the
;; linked record: display, email, etc.

(defun ebdb-org-store-link ()
  "Store a link to an EBDB contact."
  (when (eq major-mode 'ebdb-mode)
    (let* ((rec (ebdb-current-record))
	   (uuid (ebdb-record-uuid rec))
	   (name (ebdb-record-name rec))
	   (link (format "ebdb:uuid/%s" uuid)))
      (org-store-link-props :type "ebdb" :name name
			    :link link :description name)
      link)))

(defun ebdb-org-open (link)
  "Follow a EBDB link."
  (let ((records
	 (pcase (split-string link "/" t)
	   (`("uuid" ,key) (list (ebdb-gethash key 'uuid)))
	   (`(,key) (ebdb-search (ebdb-records) `((ebdb-field-name ,key))))
	   (`("mail" ,key) (ebdb-search (ebdb-records) `((ebdb-field-mail ,key))))
	   (`("phone" ,key) (ebdb-search (ebdb-records) `((ebdb-field-phone ,key))))
	   (`("address" ,key) (ebdb-search (ebdb-records) `((ebdb-field-address ,key))))
	   (`("notes" ,key) (ebdb-search (ebdb-records) `((ebdb-field-notes ,key))))
	   (`("tags" ,key) (ebdb-search (ebdb-records) `((ebdb-org-field-tags ,key))))
	   (`(,(and field (guard (child-of-class-p (intern-soft field) 'ebdb-field))) ,key)
	    (ebdb-search (ebdb-records) `((,(intern-soft field) ,key))))
	   (`(,other _) (error "Unknown field search prefix: %s" other)))))
    (if records
	(ebdb-display-records records nil nil nil (ebdb-popup-window))
      (message "No records found"))))

(defun ebdb-org-export (path desc format)
  "Create the export version of a EBDB link specified by PATH or DESC.
If exporting to either HTML or LaTeX FORMAT the link will be
italicized, in all other cases it is left unchanged."
  (when (string= desc (format "ebdb:%s" path))
    (setq desc path))
  (cond
   ((eq format 'html) (format "<i>%s</i>" desc))
   ((eq format 'latex) (format "\\textit{%s}" desc))
   ((eq format 'odt)
    (format "<text:span text:style-name=\"Emphasis\">%s</text:span>" desc))
   (t desc)))

(defvar ebdb-org-tags nil
  "Variable holding tags defined for EBDB records.

This list is added to the result of
`org-global-tags-completion-table' when producing a list of
potential tags for completion.")

(push '(ebdb-org-field-tags ":" ":") ebdb-separator-alist)


(defclass ebdb-org-field-tags (ebdb-field-user)
  ((tags
    :type (list-of string)
    :initarg :tags
    :custom (repeat string)
    :initform nil))
  :human-readable "org tags")

(cl-defmethod ebdb-string ((field ebdb-org-field-tags))
  (ebdb-concat 'ebdb-org-field-tags (slot-value field 'tags)))

(cl-defmethod ebdb-read ((field (subclass ebdb-org-field-tags)) &optional slots obj)
  (let* ((crm-separator (cadr (assq 'ebdb-org-field-tags ebdb-separator-alist)))
	 (val (completing-read-multiple
	       "Tags: "
	       (append (org-global-tags-completion-table)
		       (when ebdb-org-tags
			 (mapcar #'list ebdb-org-tags)))
	       nil nil
	       (when obj (ebdb-string obj)))))
    (cl-call-next-method field (plist-put slots :tags val))))

(cl-defmethod ebdb-search-read ((_class (subclass ebdb-org-field-tags)))
  (let ((crm-separator (cadr (assq 'ebdb-org-field-tags ebdb-separator-alist))))
    (completing-read-multiple
     "Search for tags: "
     (append (org-global-tags-completion-table)
	     (when ebdb-org-tags
	       (mapcar #'list ebdb-org-tags)))
     nil nil)))

(cl-defmethod ebdb-field-search ((field ebdb-org-field-tags) (tag-list list))
  (catch 'found
    (dolist (tag (slot-value field 'tags) nil)
      (dolist (tt tag-list)
       (when (string-match-p tt tag)
	 (throw 'found t))))))

(cl-defmethod ebdb-init-field ((field ebdb-org-field-tags) _record)
  (let ((tags (slot-value field 'tags)))
    (dolist (tag tags)
      (add-to-list 'ebdb-org-tags tag))))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode org-mode))
  "Use a separate EBDB buffer for Org-related contacts."
  (format "*%s-Org*" ebdb-buffer-name))

(provide 'ebdb-org)
;;; ebdb-org.el ends here
