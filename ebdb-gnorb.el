;;; ebdb-gnorb.el --- Utilities for connecting EBDB to Gnorb  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

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

;; Bits and pieces useful for tying EBDB in with Gnorb.  This file
;; will eventually be moved to the Gnorb package.

;;; Code:

(cl-defstruct gnorb-ebdb-link
  subject date group id)

(defvar gnorb-ebdb-org-tags nil
  "Variable holding tags defined for EBDB records.

This list is added to the result of
`org-global-tags-completion-table' when producing a list of
potential tags for completion.")

(push '(gnorb-ebdb-field-tags ";" ";") ebdb-separator-alist)

(defclass gnorb-ebdb-field-messages (ebdb-field-user)
  ((messages
    :type (list-of gnorb-ebdb-link)
    :initarg :messages
    :initform nil))
  :human-readable "gnorb messages")

(cl-defmethod ebdb-string ((_field gnorb-ebdb-field-messages))
  "Some messages")

(defcustom gnorb-ebdb-org-tag-field 'org-tags
  "The name (as a symbol) of the field to use for org tags."
  :group 'gnorb-ebdb
  :type 'symbol)

(defclass gnorb-ebdb-field-tags (ebdb-field-user)
  ((tags
    :type (list-of string)
    :initarg :tags
    :custom (repeat string)
    :initform nil))
  :human-readable "gnorb tags")

(cl-defmethod ebdb-string ((field gnorb-ebdb-field-tags))
  (ebdb-concat 'gnorb-ebdb-field-tags (slot-value field 'tags)))

(cl-defmethod ebdb-read ((field (subclass gnorb-ebdb-field-tags)) &optional slots obj)
  (let* ((crm-separator (cadr (assq 'gnorb-ebdb-field-tags ebdb-separator-alist)))
	 (val (completing-read-multiple
	       "Tags: "
	       (append (org-global-tags-completion-table)
		       (when gnorb-ebdb-org-tags
			 (mapcar #'list gnorb-ebdb-org-tags)))
	       nil nil
	       (when obj (ebdb-string obj)))))
    (cl-call-next-method field (plist-put slots :tags val))))

(cl-defmethod ebdb-init-field ((field gnorb-ebdb-field-tags) _record)
  (let ((tags (slot-value field 'tags)))
    (dolist (tag tags)
      (add-to-list 'gnorb-ebdb-org-tags tag))))

(provide 'ebdb-gnorb)
