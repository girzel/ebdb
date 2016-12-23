;;; ebdb-org.el --- Org mode integration for EBDB    -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Eric Abrahamsen

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

;; EBDB links can come in several varieties.  A plain string is simply
;; fed directly to `bbdb-search'.  Otherwise, the string can be
;; prefixed with a field type, to search only on those field values.
;; The prefix is separated with a forward slash.  Examples:

;; 1. "ebdb:uuid/af1373d6-4ba1-46a7-aa4b-845db01bc2ab" (link to unique
;; record)

;; 2. "ebdb:mail/google.com" (all records with google.com email
;; addresses)

;; 3. "ebdb:ebdb-field-foobar/baz" (search on a particular field
;; class)

;; Valid prefixes include all the values accepted by
;; `ebdb-record-field', as well as the names of field classes.

;; When calling `org-store-link' on a contact, a "ebdb:uuid/" style
;; link is created by default.

;;; Code:

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "ebdb"
			     :follow 'ebdb-org-open
			     :complete (lambda ()
					 (format
					  "ebdb:uuid/%s"
					  (ebdb-record-uuid (ebdb-completing-read-record "Record: "))))
			     :store 'ebdb-org-store-link
			     :export 'ebdb-org-export)
  (org-add-link-type "ebdb" #'ebdb-org-open #'ebdb-org-export)
  (add-hook 'org-store-link-functions 'ebdb-org-store-link))

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
  (let ((bits (split-string link "/" t))
	records)
    (if (string-match-p "^ebdb-field-" (car bits))
	(message "Following field type links not implemented yet.")
      (setq records
	    (pcase bits
	      (`("uuid" ,key) (list (ebdb-gethash key 'uuid)))
	      (`(,key) (ebdb-search (ebdb-records) key))
	      (`("mail" ,key) (ebdb-search (ebdb-records) nil nil key))
	      (`("phone" ,key) (ebdb-search (ebdb-records) nil nil nil nil key))
	      (`("address" ,key) (ebdb-search (ebdb-records) nil nil nil nil nil key))
	      (_ 'unknown)))
      (cond
	((eql records 'unknown) (message "Unknown field prefix: %s" (nth 1 bits)))
	((null records) (message "No records found"))
	(t (ebdb-display-records records))))))

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

(provide 'ebdb-org)
;;; ebdb-org.el ends here
