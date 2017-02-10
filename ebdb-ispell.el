;;; ebdb-ispell.el --- Add EBDB contact names to personal dictionaries  -*- lexical-binding: t; -*-

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

;; Copied from bbdb-ispell.el, originally written by Ivan Kanis.

;;; Code:

(require 'ispell)
(require 'ebdb)

(defcustom ebdb-ispell-dictionary-list '("default")
  "List of ispell personal dictionaries.
Allowed elements are as in the return value of `ispell-valid-dictionary-list'."
  :group 'ebdb-utilities-ispell
  :type (cons 'set (mapcar (lambda (dict) `(string ,dict))
                           (ispell-valid-dictionary-list))))

(defcustom ebdb-ispell-field-list '(name organization aka)
  "List of fields of each EBDB record considered for the personal dictionary."
  :group 'ebdb-utilities-ispell
  :type (list 'repeat
              (append '(choice) (mapcar (lambda (field) `(const ,field))
                                        '(name organization affix aka address))
                      '((symbol :tag "xfield")))))

(defcustom ebdb-ispell-min-word-length 3
  "Words with fewer characters are ignored."
  :group 'ebdb-utilities-ispell
  :type 'number)

(defcustom ebdb-ispell-ignore-re "[^[:alpha:]]"
  "Words matching this regexp are ignored."
  :group 'ebdb-utilities-ispell
  :type 'regexp)

;; Internal variable
(defvar ebdb-ispell-word-list nil
  "List of words extracted from the EBDB records.")

;;;###autoload
(defun ebdb-ispell-export ()
  "Export EBDB records to ispell personal dictionaries."
  (interactive)
  (message "Exporting to personal dictionary...")
  (let (ebdb-ispell-word-list)
    ;; Collect words from EBDB records.
    (dolist (record (ebdb-records))
      (dolist (field ebdb-ispell-field-list)
        (ebdb-ispell-collect-words (ebdb-record-field record field))))

    ;; Update personal dictionaries
    (dolist (dict (or ebdb-ispell-dictionary-list '("default")))
      (ispell-change-dictionary dict)
      ;; Initialize variables and dicts alists
      (ispell-set-spellchecker-params)
      (ispell-init-process)
      ;; put in verbose mode
      (ispell-send-string "%\n")
      (let (new)
        (dolist (word (delete-dups ebdb-ispell-word-list))
          (ispell-send-string (concat "^" word "\n"))
          (while (progn
                   (ispell-accept-output)
                   (not (string= "" (car ispell-filter)))))
          ;; remove extra \n
          (setq ispell-filter (cdr ispell-filter))
          (when (and ispell-filter
                     (listp ispell-filter)
                     (not (eq (ispell-parse-output (car ispell-filter)) t)))
            ;; ok the word doesn't exist, add it
            (ispell-send-string (concat "*" word "\n"))
            (setq new t)))
        (when new
          ;; Save dictionary:
          ;; aspell doesn't tell us when it completed the saving.
          ;; So we send it another word for spellchecking.
          (ispell-send-string "#\n^hello\n")
          (while (progn
                   (ispell-accept-output)
                   (not (string= "" (car ispell-filter)))))))))
  (message "Exporting to personal dictionary...done"))

(defun ebdb-ispell-collect-words (field)
  "Parse EBDB FIELD and collect words in `ebdb-ispell-word-list'."
  ;; Ignore everything in FIELD that is not a string or a sequence.
  (cond ((stringp field)
         (dolist (word (split-string field))
           (if (and (>= (length word) ebdb-ispell-min-word-length)
                    (not (string-match ebdb-ispell-ignore-re word)))
               (push word ebdb-ispell-word-list))))
        ((sequencep field) (mapc 'ebdb-ispell-collect-words field))))

(provide 'ebdb-ispell)
;;; ebdb-ispell.el ends here
