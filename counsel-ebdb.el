;;; counsel-ebdb.el --- Counsel integration for EBDB  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Version: 1
;; Package-Requires: ((ivy "0.8.0") (ebdb "0.2"))

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

;; Counsel (actually ivy) integration for EBDB.

;;; Code:

(require 'ebdb)
(require 'ivy)

(defun counsel-ebdb ()
  "Select EBDB contacts using the ivy/counsel interface."
  (interactive)
  (ivy-read
   "Records: "
   (mapcar
    ;; This same lambda is used in helm-ebdb, refactor or maybe even
    ;; make customizable.  Presumably we could use the :matcher
    ;; argument to provide a function that matched the name and mail
    ;; strings, but then you wouldn't actually see the mail strings in
    ;; the completion window, would you?
    (lambda (rec)
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
    (ebdb-records))
   :action
   '(1
     ("o" (lambda (r)
	    (ebdb-display-records (list (cdr r)) nil t)) "display")
     ("m" (lambda (r) (ebdb-mail (cdr r))) "send mail")
     ("i" (lambda (r) (ebdb-cite-records-mail (cdr r))) "insert"))))

(provide 'counsel-ebdb)
;;; counsel-ebdb.el ends here
