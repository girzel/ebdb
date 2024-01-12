;;; ebdb-roam.el --- Org-Roam integration for EBDB   -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
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

;;; Code:

(require 'org-roam-node)
(require 'ebdb)
(require 'ebdb-format)


;; org-roam-buffer Section

(defun ebdb-roam--get-element-for (node)
  "Get the element containing NODE."
  (cl-do ((element (with-current-buffer (find-buffer-visiting (org-roam-node-file node))
                     (save-excursion
                       (org-with-wide-buffer
                        (goto-char (org-roam-node-point node))
                        (org-element-at-point))))
                   (org-element-parent element)))
      ((org-element-type-p element 'section) elem)))

(defun ebdb-roam--get-links (node)
  "Get non-reference EBDB links for NODE."
  (let* ((bare-ref (car (org-roam-node-refs node)))
         (link (and (stringp bare-ref)
                    (string-match-p "^uuid/" bare-ref)
                    (substring bare-ref 5)))
         (element (ebdb-roam--get-element-for node)))
    (cl-remove-duplicates
     (cl-remove-if-not #'stringp
                       (cons link
                             (org-element-map element 'link
                               (lambda (link)
                                 (when-let ((link-type-p (string= "ebdb" (org-element-property :type link)))
                                            (uuid-link (string-match-p (rx eol "uuid/") (org-element-property :path link))))
                                   (substring uuid-link 5))))))
     :test #'string=)))

(defun ebdb-roam-section (node)
  "Show EBDB entries for current NODE."
  (when-let ((references (ebdb-roam--get-links node))
             (entries (delq nil
                            (mapcar (lambda (reference)
                                      (ebdb-gethash reference 'uuid))
                                    references))))
    (magit-insert-section (org-roam-ebdb-section)
      (magit-insert-heading "Address Book Entries")
      (dolist (entry entries)
        (insert (ebdb-fmt-record ebdb-default-multiline-formatter entry))
        (insert "\n\n")))))

(provide 'ebdb-roam)
;;; ebdb-roam.el ends here
