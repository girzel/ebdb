;;; ebdb-message.el --- EBDB interface to mail composition packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2020  Free Software Foundation, Inc.

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
(require 'ebdb-com)
(require 'message)
(require 'sendmail)

(defvar gnus-window-to-buffer)

(defgroup ebdb-mua-message nil
  "Message-specific EBDB customizations"
  :group 'ebdb-mua)
(put 'ebdb-mua-message 'custom-loads '(ebdb-message))

(defcustom ebdb-message-auto-update-p ebdb-mua-sender-update-p
  "Message-specific value of `ebdb-mua-auto-update-p'."
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" existing)
                 (const :tag "update existing records" update)
                 (const :tag "query for update or record creation" query)
                 (const :tag "update or create automatically" create)
                 (function :tag "User-defined function")))

(defcustom ebdb-message-window-size ebdb-default-window-size
  "Size of the EBDB buffer when popping up in message-mode.
Size should be specified as a float between 0 and 1.  Defaults to
the value of `ebdb-default-window-size'."
  :type 'float)

(defcustom ebdb-message-reply-window-config
  `(reply
    (horizontal 1.0
		(message 1.0 point)
		(ebdb-message ,ebdb-message-window-size)))
  "Message reply window configuration to show EBDB.
See Gnus' manual for details."
  :group 'ebdb-mua-message
  :type 'list)

(defcustom ebdb-message-reply-yank-window-config
  `(reply-yank
     (horizontal 1.0
		 (message 1.0 point)
		 (ebdb-message ,ebdb-message-window-size)))
  "Message reply-yank window configuration to show EBDB.
See Gnus' manual for details."
  :group 'ebdb-mua-message
  :type 'list)

;; Suggestions welcome: What are good keybindings for the following
;; commands that do not collide with existing bindings?
;; (define-key message-mode-map "'" 'ebdb-mua-display-recipients)
;; (define-key message-mode-map ";" 'ebdb-mua-edit-field-recipients)
;; (define-key message-mode-map "/" 'ebdb)

(defsubst ebdb-message-buffer-name ()
  (format "*%s-Message*" ebdb-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode message-mode))
  "Produce a EBDB buffer name associated with Message mode."
  (ebdb-message-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode mail-mode))
  "Produce a EBDB buffer name associated with Mail mode."
  (ebdb-message-buffer-name))

(cl-defmethod ebdb-mua-message-header ((header string)
				    &context (major-mode message-mode))
  (message-field-value header))

(cl-defmethod ebdb-mua-message-header ((header string)
				    &context (major-mode notmuch-message-mode))
  (message-field-value header))

(cl-defmethod ebdb-mua-message-header ((header string)
				    &context (major-mode mail-mode))
  (message-field-value header))

(cl-defmethod ebdb-popup-window (&context (major-mode message-mode))
  (list (get-buffer-window) ebdb-message-window-size))

(cl-defmethod ebdb-popup-window (&context (major-mode mail-mode))
  (list (get-buffer-window) ebdb-message-window-size))

(defun ebdb-message-complete-mail-cleanup (str _buffer pos &rest _)
  "Call `ebdb-complete-mail-cleanup' after capf completion."
  (ebdb-complete-mail-cleanup str pos))

(defun ebdb-message-quit-ebdb ()
  "Remove the EBDB window if the user kills the message buffer.
Also fires when postponing a draft."
  (let ((win (get-buffer-window (ebdb-message-buffer-name))))
    (when win
      (quit-window nil win))))

(defun ebdb-insinuate-message ()
  ;; We don't currently bind the `ebdb-mua-keymap'.
  (pcase ebdb-complete-mail
    ('capf (progn (add-hook
		   'completion-at-point-functions
  		   #'ebdb-mail-dwim-completion-at-point-function nil t)
		  ;; Kind of hacky way of mimicking
		  ;; `ebdb-complete-mail' behavior, but for capf.
		  (add-hook
		   'choose-completion-string-functions
		   #'ebdb-message-complete-mail-cleanup
		   nil t)))
    ('nil nil)
    (_
     (cl-pushnew '("^\\(Resent-\\)?\\(To\\|B?Cc\\|Reply-To\\|From\\|Mail-Followup-To\\|Mail-Copies-To\\):" . ebdb-complete-mail)
  		 message-completion-alist
  		 :test #'equal)))
  (message-add-action
   #'ebdb-message-quit-ebdb 'exit 'postpone 'kill)
  ;; Other MUAs clear the EBDB buffer before displaying (in
  ;; `ebdb-mua-auto-update', the call to `ebdb-display-records' does
  ;; not pass the "append" flag).  Displaying in message-mode does
  ;; pass the "append" flag (in `ebdb-complete-mail-cleanup'), so we
  ;; do the undisplay manually.
  (ebdb-undisplay-records))

(defun ebdb-insinuate-mail ()
  "Hook EBDB into Mail Mode."
  ;; We don't currently bind the `ebdb-mua-keymap'.
  (pcase ebdb-complete-mail
    ('capf (progn (add-hook
		   'completion-at-point-functions
  		   #'ebdb-mail-dwim-completion-at-point-function nil t)
		  ;; See above.
		  (add-hook
		   'choose-completion-string-functions
		   #'ebdb-message-complete-mail-cleanup
		   nil t)))
    ('nil nil)
    (_ (define-key mail-mode-map "\M-\t" 'ebdb-complete-mail)))
  (ebdb-undisplay-records))

(defun ebdb-message-auto-update ()
  (ebdb-mua-auto-update ebdb-message-auto-update-p))

(add-hook 'message-mode-hook 'ebdb-insinuate-message)
(add-hook 'mail-setup-hook 'ebdb-insinuate-mail)
(add-hook 'message-send-hook 'ebdb-message-auto-update)
(add-hook 'mail-send-hook 'ebdb-message-auto-update)

;; Slightly convoluted, but does it the "right way".  The
;; `message-header-setup-hook' creates and populates the
;; *EBDB-Message* buffer after the message-mode buffer is created.
;; The gnus window configuration stanza makes sure it's displayed
;; after the message buffer is set up.
(add-hook 'ebdb-after-load-hook
	  (lambda ()
	    (with-eval-after-load "gnus-win"
	      (add-hook 'message-header-setup-hook 'ebdb-message-auto-update)

	      (when ebdb-mua-pop-up
		(add-to-list 'gnus-window-to-buffer
			     `(ebdb-message . ,(ebdb-message-buffer-name)))

		(gnus-add-configuration
		 ebdb-message-reply-window-config)

		(gnus-add-configuration
		 ebdb-message-reply-yank-window-config)))))

(provide 'ebdb-message)
;;; ebdb-message.el ends here
