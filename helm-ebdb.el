;;; helm-ebdb.el --- Helm integration for EBDB       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: mail, convenience
;; Version: 1
;; Package-Requires: ((helm "1.0") (ebdb "0.2"))

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

;; Helm integration for EBDB.

;;; Code:

(require 'ebdb)
(require 'helm)

(declare-function ebdb-display-records "ext:ebdb-com"
		  (records &optional fmt append select pop buf))

(defun helm-ebdb-candidates ()
  "Return a list of all records in the database."
  (mapcar (lambda (rec)
	    (let* ((rec-string (ebdb-string rec))
		   (mails (ebdb-record-mail-canon rec))
		   (mail-list (when mails
				(mapconcat #'identity
					   mails
					   " "))))
	      (cons (if mail-list
			(concat rec-string
				" => "
				mail-list)
		      rec-string)
		    rec)))
	  (ebdb-records)))

(defun helm-ebdb-display-records (candidate)
  "Display CANDIDATE or marked candidates."
  (let ((recs (or (helm-marked-candidates) (list candidate))))
    (ebdb-display-records recs nil nil t nil
			  (format "*%s*" ebdb-buffer-name))))

(defun helm-ebdb-compose-mail (candidate)
  "Compose mail to CANDIDATE or marked candidates."
  (let ((recs (or (helm-marked-candidates) (list candidate))))
    (ebdb-mail recs nil t)))

(defun helm-ebdb-cite-records (candidate)
  "Insert Name <email> string for CANDIDATE or marked candidate."
  (let ((recs (or (helm-marked-candidates) (list candidate))))
    (ebdb-cite-records recs)))

(defvar helm-source-ebdb
  '((name . "EBDB")
    (candidates . helm-ebdb-candidates)
    (action . (("Display" . helm-ebdb-display-records)
	       ("Send mail" . helm-ebdb-compose-mail)
	       ("Insert name and address" . helm-ebdb-cite-records)))))

;;;###autoload
(defun helm-ebdb ()
  "Preconfigured `helm' for EBDB."
  (interactive)
  (helm-other-buffer 'helm-source-ebdb "*helm ebdb*"))

(provide 'helm-ebdb)
;;; helm-ebdb.el ends here
