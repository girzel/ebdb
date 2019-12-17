;;; ebdb-notmuch.el --- EBDB interface to Notmuch    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; EBDB's interface to the Notmuch mail client.

;;; Code:

(require 'ebdb-mua)

(declare-function notmuch-show-get-header "notmuch-show")
(defvar notmuch-show-mode-map)
(defvar notmuch-message-mode-map)

(defgroup ebdb-mua-notmuch nil
  "Options for EBDB's interaction with Notmuch"
  :group 'ebdb-mua)

(defcustom ebdb-notmuch-auto-update-p ebdb-mua-reader-update-p
  "Notmuch-specific value of `ebdb-mua-auto-update-p'."
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" existing)
                 (const :tag "update existing records" update)
                 (const :tag "query for update or record creation" query)
                 (const :tag "update or create automatically" create)
                 (function :tag "User-defined function")))

(defcustom ebdb-notmuch-window-size ebdb-default-window-size
  "Size of the EBDB buffer when popping up in Notmuch.
Size should be specified as a float between 0 and 1.  Defaults to
the value of `ebdb-default-window-size'."
  :type 'float)

(cl-defmethod ebdb-mua-message-header ((header string)
				       &context (major-mode notmuch-show-mode))
  "Extract a message header in Notmuch."
  (notmuch-show-get-header
   ;; Yuck, is there no better way to turn a string into a keyword?
   (intern (format ":%s" (capitalize header)))))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode notmuch-show-mode))
  (format "*%s-Notmuch*" ebdb-buffer-name))

(cl-defmethod ebdb-popup-window (&context (major-mode notmuch-show-mode))
  (list (get-buffer-window) ebdb-notmuch-window-size))

(defun ebdb-insinuate-notmuch ()
  "Hook EBDB into Notmuch."
  (define-key notmuch-show-mode-map ";" ebdb-mua-keymap)
  (when ebdb-complete-mail
    (with-eval-after-load "notmuch-message"
      (define-key notmuch-message-mode-map (kbd "TAB") #'ebdb-complete-mail))))

(add-hook 'notmuch-show-mode-hook #'ebdb-insinuate-notmuch)

(provide 'ebdb-notmuch)
;;; ebdb-notmuch.el ends here
