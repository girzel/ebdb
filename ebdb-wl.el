;;; ebdb-wl.el --- EBDB interface to Wanderlust  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; EBDB's interface to the Wanderlust email client.

;;; Code:

(require 'ebdb-mua)

(autoload 'elmo-message-entity-field "ext:elmo-msgdb")
(autoload 'elmo-message-entity "ext:elmo")
(autoload 'wl-summary-message-number "ext:wl-summary")

(defvar wl-current-summary-buffer)
(defvar wl-summary-buffer-elmo-folder)
(defvar wl-message-buffer)
(defvar wl-summary-mode-map)

(defgroup ebdb-wl nil
  "Options for EBDB's interaction with Wanderlust."
  :group 'ebdb-mua)

;; This rebinds <TAB> in `wl-draft-mode-map' to `ebdb-complete-mail'.
;; WL has its own completion mechanism that we could hook into, by
;; setting `wl-address-init-function' to our own function that
;; populates `wl-address-completion-list', but that would mean that
;; we're basically duplicating most of the information in the EBDB,
;; and `ebdb-complete-mail' works fine in `wl-draft-mode'.
(defcustom ebdb-wl-use-ebdb-completion nil
  "If non-nil, use EBDB mail completion in WL draft mode."
  :group 'ebdb-wl
  :type 'bool)

(cl-defmethod ebdb-mua-message-header ((header string)
				       &context (major-mode mime-view-mode))
  "Extract a message header in Wanderlust."
  (elmo-message-entity-field
   ;; It's possibly not safe to assume `wl-current-summary-buffer' is live?
   (with-current-buffer wl-current-summary-buffer
     (elmo-message-entity wl-summary-buffer-elmo-folder
			  (wl-summary-message-number)))
   (intern (downcase header)) 'string))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode mime-view-mode))
  (format "*%s-Wl" ebdb-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode wl-summary-mode))
  (format "*%s-Wl" ebdb-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode wl-draft-mode))
  (format "*%s-Wl-Draft" ebdb-buffer-name))

(cl-defmethod ebdb-popup-window (&context (major-mode mime-view-mode))
  (list (get-buffer-window) 0.3))

(defun ebdb-insinuate-wl ()
  "Hook EBDB into Wanderlust."
  (define-key wl-summary-mode-map ";" ebdb-mua-keymap)
  (when ebdb-wl-use-ebdb-completion
    (define-key wl-draft-mode-map (kbd "TAB") #'ebdb-complete-mail)))

(add-hook 'wl-folder-mode-hook #'ebdb-insinuate-wl)

(add-hook 'wl-message-redisplay-hook #'ebdb-mua-auto-update)

(provide 'ebdb-wl)
;;; ebdb-wl.el ends here
