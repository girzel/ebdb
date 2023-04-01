;;; ebdb-roam.el --- Org-Roam integration for EBDB   -*- lexical-binding: t; -*-

;; Author: Samuel W. Flint <swflint@flintfam.org>
;; Keywords

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

(defun ebdb-org-roam-section (node)
  "Show EBDB entry for current NODE."
  (when-let ((bare-ref (car (org-roam-node-refs node)))
             (reference (when (string-match-p "^uuid/" bare-ref)
                          (substring bare-ref 5)))
             (entry (ebdb-gethash reference 'uuid)))
    (magit-insert-section (org-roam-ebdb-section)
      (magit-insert-heading "Address Book Entry")
      (insert (ebdb-fmt-record ebdb-default-multiline-formatter entry))
      (insert "\n\n"))))

(provide 'ebdb-roam)
;;; ebdb-roam.el ends here
