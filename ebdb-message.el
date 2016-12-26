;;; ebdb-message.el --- EBDB interface to mail composition packages  -*- lexical-binding: t; -*-

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

;; Code for interaction with message composition and sending packages.

;;; Code:


(require 'ebdb-mua)
(require 'message)
(require 'sendmail)

(defgroup ebdb-mua-message nil
  "Message-specific EBDB customizations"
  :group 'ebdb-mua)
(put 'ebdb-mua-message 'custom-loads '(ebdb-message))

;; Suggestions welcome: What are good keybindings for the following
;; commands that do not collide with existing bindings?
;; (define-key message-mode-map "'" 'ebdb-mua-display-recipients)
;; (define-key message-mode-map ";" 'ebdb-mua-edit-field-recipients)
;; (define-key message-mode-map "/" 'ebdb)

(cl-defmethod ebdb-make-buffer-name (&context (major-mode message-mode))
  "Produce a EBDB buffer name associated with Message mode."
  (format "*%s-Message*" ebdb-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode mail-mode))
  "Produce a EBDB buffer name associated with Mail mode."
  (format "*%s-Message*" ebdb-buffer-name))

(cl-defgeneric ebdb-message-header ((header string)
				    &context (major-mode message-mode))
  (message-field-value header))

(cl-defgeneric ebdb-message-header ((header string)
				    &context (major-mode notmuch-message-mode))
  (message-field-value header))

(cl-defgeneric ebdb-message-header ((header string)
				    &context (major-mode mail-mode))
  (message-field-value header))

(defun ebdb-insinuate-message ()
  (when ebdb-complete-mail
    (cl-pushnew '("^\\(Resent-\\)?\\(To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):" . ebdb-complete-mail)
		message-completion-alist
		:test #'equal)
    (define-key mail-mode-map (kbd "TAB") 'ebdb-complete-mail)))

(defun bbdb-insinuate-mail ()
  "Hook BBDB into Mail Mode.
Do not call this in your init file.  Use `bbdb-initialize'."
  ;; Suggestions welcome: What are good keybindings for the following
  ;; commands that do not collide with existing bindings?
  ;; (define-key mail-mode-map "'" 'bbdb-mua-display-recipients)
  ;; (define-key mail-mode-map ";" 'bbdb-mua-edit-field-recipients)
  ;; (define-key mail-mode-map "/" 'bbdb)
  (if bbdb-complete-mail
      (define-key mail-mode-map "\M-\t" 'bbdb-complete-mail)))

(add-hook 'message-mode-hook 'ebdb-insinuate-message)
(add-hook 'mail-setup-hook 'ebdb-insinuate-mail)
(add-hook 'message-send-hook 'ebdb-mua-auto-update)
(add-hook 'mail-send-hook 'ebdb-mua-auto-update)
(provide 'ebdb-message)
;;; ebdb-message.el ends here
