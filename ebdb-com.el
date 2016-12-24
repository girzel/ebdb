;;; ebdb-com.el --- User-level commands of EBDB      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: convenience, mail

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

;; This file contains most of the user-level interactive commands for
;; EBDB, including the *EBDB* buffer and ebdb-mode.

;;; Code:

(require 'ebdb)
(require 'ebdb-format)
(require 'mailabbrev)

(eval-and-compile
  (autoload 'build-mail-aliases "mailalias")
  (autoload 'browse-url-url-at-point "browse-url"))

(require 'crm)
(defvar ebdb-crm-local-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map crm-local-completion-map)
    (define-key map " " 'self-insert-command)
    map)
  "Keymap used for EBDB crm completions.")

;; Customizations for display routines

(defgroup ebdb-record-display nil
  "Variables that affect the display of EBDB records"
  :group 'ebdb)

(defcustom ebdb-pop-up-window-size 0.5
  "Vertical size of EBDB window (vertical split).
If it is an integer number, it is the number of lines used by EBDB.
If it is a fraction between 0.0 and 1.0 (inclusive), it is the fraction
of the tallest existing window that EBDB will take over.
If it is t use `display-buffer'/`pop-to-buffer' to create the EBDB window.
See also `ebdb-mua-pop-up-window-size'."
  :group 'ebdb-record-display
  :type '(choice (number :tag "EBDB window size")
                 (const :tag "Use `pop-to-buffer'" t)))

(defcustom ebdb-horiz-pop-up-window-size '(112 . 0.3)
  "Horizontal size of a pop-up EBDB window (horizontal split).
It is a cons pair (TOTAL . EBDB-SIZE).
The window that will be considered for horizontal splitting must have
at least TOTAL columns. EBDB-SIZE is the horizontal size of the EBDB window.
If it is an integer number, it is the number of columns used by EBDB.
If it is a fraction between 0 and 1, it is the fraction of the
window width that EBDB will take over."
  :group 'ebdb-mua
  :type '(cons (number :tag "Total number of columns")
               (number :tag "Horizontal size of EBDB window")))

(defcustom ebdb-dedicated-window nil
  "Make *EBDB* window a dedicated window.
Allowed values include nil (not dedicated) 'ebdb (weakly dedicated)
and t (strongly dedicated)."
  :group 'ebdb-record-display
  :type '(choice (const :tag "EBDB window not dedicated" nil)
                 (const :tag "EBDB window weakly dedicated" ebdb)
                 (const :tag "EBDB window strongly dedicated" t)))

(defcustom ebdb-fill-field-values 't
  "If t, fill particularly long field values so that they fit
within the *EBDB* buffer."
  :group 'ebdb-record-display
  ;; TODO: When we have a 'pop-up and 'user buffer distinction, give
  ;; this option more potential values.
  :type '(choice (const :tag "Always fill" nil)
                 (const :tag "Never fill" t)))

(defcustom ebdb-user-menu-commands nil
  "User defined menu entries which should be appended to the EBDB menu.
This should be a list of menu entries.
When set to a function, it is called with two arguments RECORD and FIELD
and it should either return nil or a list of menu entries.
Used by `ebdb-mouse-menu'."
  :group 'ebdb-record-display
  :type 'sexp)

(defcustom ebdb-display-hook nil
  "Hook run after the *EBDB* is filled in."
  :group 'ebdb-record-display
  :type 'hook)

(defcustom ebdb-multiple-buffers nil
  "When non-nil we create a new buffer of every buffer causing pop-ups.
You can also set this to a function returning a buffer name.
Here a value may be the predefined function `ebdb-multiple-buffers-default'."
  :group 'ebdb-record-display
  :type '(choice (const :tag "Disabled" nil)
                 (function :tag "Enabled" ebdb-multiple-buffers-default)
                 (function :tag "User defined function")))

;; Faces for font-lock
(defgroup ebdb-faces nil
  "Faces used by EBDB."
  :group 'ebdb
  :group 'faces)

(defface ebdb-person-name
  '((t (:inherit font-lock-function-name-face)))
  "Face used for EBDB person names."
  :group 'ebdb-faces)

(defface ebdb-organization-name
  '((t (:inherit font-lock-comment-face)))
  "Face used for EBDB organization names."
  :group 'ebdb-faces)

(defcustom ebdb-name-face-alist '((ebdb-record-person . ebdb-person-name)
				  (ebdb-record-organization . ebdb-organization-name))
  "Alist matching record class types to the face that should be
  used to font-lock their names in the *EBDB* buffer."
  :group 'ebdb-faces
  :type '(repeat (cons (ebdb-record :tag "Record type") (face :tag "Face"))))

(defface ebdb-marked
  '((t (:background "LightBlue")))
  "Face used for currently-marked records."
  :group 'ebdb-faces)

(defface ebdb-label
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for EBDB field labels."
  :group 'ebdb-faces)

(defface ebdb-field-url
  '((t (:inherit link)))
  "Face used for clickable links/URLs in field values."
  :group 'ebdb-faces)

(defface ebdb-defunct
  '((t (:inherit font-lock-comment-face)))
  "Face used to display defunct roles and mails."
  :group 'ebdb-faces)

(defface ebdb-mail-primary
  '((t (:inherit font-lock-builtin-face)))
  "Face used to display a record's primary mail address."
  :group 'ebdb-faces)

(defface ebdb-db-char
  '((t (:inherit shadow)))
  "Face used to display a databases's identifying character string."
  :group 'ebdb-faces)

(defvar ebdb-buffer-name "*EBDB*" "Name of the EBDB buffer.")

;;; Buffer-local variables for the database.
(defvar ebdb-records nil
  "EBDB records list.

In the *EBDB* buffers it includes the records that are actually displayed
and its elements are (RECORD DISPLAY-FORMAT MARKER-POS MARK).")
(make-variable-buffer-local 'ebdb-records)

(defvar ebdb-append-display nil
  "Controls the behavior of the command `ebdb-append-display'.")

(defvar ebdb-modeline-info (make-vector 4 nil)
  "Precalculated mode line info for EBDB commands.
This is a vector [APPEND-M APPEND INVERT-M INVERT].
APPEND-M is the mode line info if `ebdb-append-display' is non-nil.
INVERT-M is the mode line info if `ebdb-search-invert' is non-nil.")

(defun ebdb-get-records (prompt)
  "If inside the *EBDB* buffer get the current records.
In other buffers ask the user."
  (if (string= ebdb-buffer-name (buffer-name))
      (ebdb-do-records)
    (ebdb-completing-read-records prompt)))

;; Note about the arg RECORDS of various EBDB commands:
;;  - Usually, RECORDS is a list of records.  (Interactively,
;;    this list of records is set up by `ebdb-do-records'.)
;;  - If these commands are used, e.g., in `ebdb-create-hook' or
;;    `ebdb-change-hook', they will be called with one arg, a single record.
;; So depending on context the value of RECORDS will be a single record
;; or a list of records, and we want to handle both cases.
;; So we pass RECORDS to `ebdb-record-list' to handle both cases.
(defun ebdb-record-list (records &optional full)
  "Ensure that RECORDS is a list of records.
If RECORDS is a single record turn it into a list.
If FULL is non-nil, assume that RECORDS include display information."
  (if records
      (if full
          (if (vectorp (car records)) (list records) records)
        (if (vectorp records) (list records) records))))

;; Note about EBDB prefix commands:
;; `ebdb-do-all-records', `ebdb-append-display' and `ebdb-search-invert'
;; are fake prefix commands. They need not precede the main commands.
;; Also, `ebdb-append-display' can act on multiple commands.

(defun ebdb-prefix-message ()
  "Display a message about selected EBDB prefix commands."
  (let ((msg (ebdb-concat " " (elt ebdb-modeline-info 1)
                          (elt ebdb-modeline-info 3))))
    (unless (string= "" msg) (message "%s" msg))))

;;;###autoload
(defun ebdb-do-records (&optional full)
  "Return list of records to operate on.
Normally this list includes only the current record, but if any
records in the current buffer are marked, they are returned
instead.  If FULL is non-nil, the list of records includes
display information."
  (let* ((marked (seq-filter (lambda (r) (nth 3 r)) ebdb-records))
	 (recs (or marked (list (ebdb-current-record t)))))
   (if full recs (mapcar 'car recs))))

;;;###autoload
(defun ebdb-append-display-p ()
  "Return variable `ebdb-append-display' and reset."
  (let ((job (cond ((eq t ebdb-append-display))
                   ((numberp ebdb-append-display)
                    (setq ebdb-append-display (1- ebdb-append-display))
                    (if (zerop ebdb-append-display)
                        (setq ebdb-append-display nil))
                    t)
                   (ebdb-append-display
                    (setq ebdb-append-display nil)
                    t))))
    (cond ((numberp ebdb-append-display)
           (aset ebdb-modeline-info 0
                 (format "(add %dx)" ebdb-append-display)))
          ((not ebdb-append-display)
           (aset ebdb-modeline-info 0 nil)
           (aset ebdb-modeline-info 1 nil)))
    job))

;;;###autoload
(defun ebdb-append-display (&optional arg)
  "Toggle appending next searched records in the *EBDB* buffer.
With prefix ARG \\[universal-argument] always append.
With ARG a positive number append for that many times.
With ARG a negative number do not append."
  (interactive "P")
  (setq ebdb-append-display
        (cond ((and arg (listp arg)) t)
              ((and (numberp arg) (< 1 arg)) arg)
              ((or (and (numberp arg) (< arg 0)) ebdb-append-display) nil)
              (t 'once)))
  (aset ebdb-modeline-info 0
        (cond ((numberp ebdb-append-display)
               (format "(add %dx)" ebdb-append-display))
              ((eq t ebdb-append-display) "Add")
              (ebdb-append-display "add")
              (t nil)))
  (aset ebdb-modeline-info 1
        (if ebdb-append-display
            (substitute-command-keys
             "\\<ebdb-mode-map>\\[ebdb-append-display]")))
  (ebdb-prefix-message))

;;; Keymap
(defvar ebdb-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km "*"          'ebdb-do-all-records)
    (define-key km "+"          'ebdb-append-display)
    (define-key km "!"          'ebdb-search-invert)
    (define-key km "a"          'ebdb-record-action)
    (define-key km "A"          'ebdb-mail-aliases)
    (define-key km "c"          'ebdb-create-record)
    (define-key km "C"          'ebdb-create-record-extended)
    (define-key km "e"          'ebdb-edit-field)
    (define-key km ";"          'ebdb-edit-foo)
    (define-key km "n"          'ebdb-next-record)
    (define-key km "p"          'ebdb-prev-record)
    (define-key km "N"          'ebdb-next-field)
    (define-key km "\t"         'ebdb-next-field) ; TAB
    (define-key km "P"          'ebdb-prev-field)
    (define-key km "\d"         'ebdb-prev-field) ; DEL
    (define-key km "d"          'ebdb-delete-field-or-record)
    (define-key km "\C-k"       'ebdb-delete-field-or-record)
    (define-key km "i"          'ebdb-insert-field)
    (define-key km (kbd "RET")  'ebdb-follow-related)
    (define-key km "s"          'ebdb-save)
    (define-key km "\C-x\C-s"   'ebdb-save)
    (define-key km "t"          'ebdb-toggle-records-format)
    (define-key km "T"          'ebdb-display-records-completely)
    (define-key km (kbd "#")    'ebdb-toggle-record-mark)
    (define-key km "o"          'ebdb-omit-record)
    (define-key km "m"          'ebdb-mail)
    (define-key km "M"          'ebdb-mail-address)
    (define-key km "\M-d"       'ebdb-dial)
    (define-key km "h"          'ebdb-info)
    (define-key km "?"          'ebdb-help)
    ;; (define-key km "q"       'quit-window) ; part of `special-mode' bindings
    (define-key km "\C-x\C-t"   'ebdb-transpose-fields)
    (define-key km "Cr"         'ebdb-copy-records-as-kill)
    (define-key km "Cf"         'ebdb-copy-fields-as-kill)
    (define-key km "u"          'ebdb-browse-url)
    ;; (define-key km "P"       'ebdb-print)
    (define-key km "="          'delete-other-windows)

    ;; Search keys
    (define-key km "b"          'ebdb)
    (define-key km "/1"         'ebdb-display-records)
    (define-key km "/n"         'ebdb-search-name)
    (define-key km "/o"         'ebdb-search-organization)
    (define-key km "/p"         'ebdb-search-phone)
    (define-key km "/a"         'ebdb-search-address)
    (define-key km "/m"         'ebdb-search-mail)
    (define-key km "/N"         'ebdb-search-user-fields)
    (define-key km "/x"         'ebdb-search-user-fields)
    (define-key km "/c"         'ebdb-search-changed)
    (define-key km "/d"         'ebdb-search-duplicates)
    (define-key km "\C-xnw"     'ebdb-display-all-records)
    (define-key km "\C-xnd"     'ebdb-display-current-record)

    (define-key km [delete]     'scroll-down) ; 24.1: part of `special-mode'
    (define-key km " "          'scroll-up)   ; 24.1: part of `special-mode'

    (define-key km [mouse-3]    'ebdb-mouse-menu)
    (define-key km [mouse-2]    (lambda (event)
                                  ;; Toggle record format
                                  (interactive "e")
                                  (save-excursion
                                    (posn-set-point (event-end event))
                                    (ebdb-toggle-records-format
                                     (ebdb-do-records t) current-prefix-arg))))
    km)
  "Keymap for Insidious Big Brother Database.
This is a child of `special-mode-map'.")

(defun ebdb-current-record (&optional full)
  "Return the record point is at.
If FULL is non-nil record includes the display information."
  (unless (eq major-mode 'ebdb-mode)
    (error "This only works while in EBDB buffers."))
  (let ((num (get-text-property (if (and (not (bobp)) (eobp))
                                    (1- (point)) (point))
                                'ebdb-record-number))
        record)
    (unless num (error "Not a EBDB record"))
    (setq record (nth num ebdb-records))
    (if full record (car record))))

(defun ebdb-current-field ()
  "Return current field point is on."
  (unless (ebdb-current-record) (error "Not a EBDB record"))
  (or (get-text-property (point) 'ebdb-field)
      (get-text-property
       (next-single-property-change (line-beginning-position)
				    'ebdb-field nil
				    (line-end-position))
       'ebdb-field)))

;;; *EBDB* formatting

(defclass ebdb-formatter-ebdb (ebdb-formatter)
  nil
  :documentation
  "Abstract formatter base class for *EBDB* buffer(s)."
  :abstract t)

(defclass ebdb-formatter-ebdb-oneline (ebdb-formatter-ebdb)
  nil
  :documentation
  "Single line formatter for *EBDB* buffers.")

(defclass ebdb-formatter-ebdb-multiline (ebdb-formatter-ebdb)
  nil
  :documentation
  "Multi-line formatter for *EBDB* buffers.")

(defcustom ebdb-default-multiline-formatter
  (make-instance 'ebdb-formatter-ebdb-multiline
		 :object-name "multiline formatter")
  "The default multiline formatter for *EBDB* buffers."
  :type 'ebdb-formatter-ebdb-multiline
  :group 'ebdb-record-display)

(defcustom ebdb-default-oneline-formatter
  (make-instance 'ebdb-formatter-ebdb-oneline
		 :object-name "oneline formatter"
		 :include '(ebdb-field-mail))
  "The default oneline formatter of *EBDB* buffers."
  :type 'ebdb-formatter-ebdb-oneline
  :group 'ebdb-record-display)

(defun ebdb-available-ebdb-formatters ()
  "A list of formatters available in the *EBDB* buffer.

This list is also used for toggling layouts."
  (seq-filter
   (lambda (f) (object-of-class-p f 'ebdb-formatter-ebdb))
   ebdb-formatter-tracker))

(defsubst ebdb-formatter-prefix ()
  "Select a formatter interactively using the prefix arg."
  (cond (current-prefix-arg ebdb-default-oneline-formatter)
	(t ebdb-default-multiline-formatter)))

;; *EBDB* buffer formatting.

(cl-defmethod ebdb-record-db-char-string ((record ebdb-record))
  (let* ((dbs (slot-value (ebdb-record-cache record) 'database))
	 (char-string
	  (copy-sequence
	   (mapconcat
	    (lambda (d)
	      (when (slot-value d 'buffer-char)
		(slot-value d 'buffer-char)))
	    dbs ""))))
    (add-face-text-property 0 (length char-string) 'ebdb-db-char nil char-string)
    char-string))

(cl-defmethod ebdb-fmt-field-label ((_fmt ebdb-formatter-ebdb)
				    (field ebdb-field-phone)
				    (_style (eql oneline))
				    (_record ebdb-record))
  (format "phone (%s)" (eieio-object-name-string field)))

(cl-defmethod ebdb-fmt-field-label ((_fmt ebdb-formatter-ebdb)
				    (field ebdb-field-address)
				    (_style (eql oneline))
				    (_record ebdb-record))
  (format "address (%s)" (eieio-object-name-string field)))

;; In these methods, `copy-sequence' is used because otherwise the
;; text property is actually set on the field's slot value itself
;; (which leads to it getting written to the database, ugh).

(cl-defmethod ebdb-fmt-field :around ((_fmt ebdb-formatter-ebdb)
				      (field ebdb-field)
				      _style
				      (_record ebdb-record))
  "Put the 'ebdb-field text property on FIELD.  The value of the
property is the field instance itself."
  (let ((val-string (copy-sequence (cl-call-next-method))))
    (put-text-property 0 (length val-string) 'ebdb-field field val-string)
    val-string))

(cl-defmethod ebdb-fmt-field ((_fmt ebdb-formatter-ebdb)
			      (field ebdb-field-url)
			      _style
			      (_record ebdb-record))
  "Add an appropriate face to url fields."
  (let ((value (copy-sequence (ebdb-string field))))
    (add-face-text-property 0 (length value) 'ebdb-field-url nil value)
    value))

(cl-defmethod ebdb-fmt-field ((_fmt ebdb-formatter-ebdb)
			      (field ebdb-field-mail)
			      _style
			      (_record ebdb-record))
  "Add an appropriate face to primary and defunct mails."
  (let* ((priority (slot-value field 'priority))
	 (value (copy-sequence (ebdb-string field)))
	 (face (cond
		((eq priority 'primary) 'ebdb-mail-primary)
		((eq priority 'defunct) 'ebdb-defunct)
		(t nil))))
    (when face
      (add-face-text-property 0 (length value) face nil value))
    value))

(cl-defmethod ebdb-fmt-field ((fmt ebdb-formatter-ebdb)
			      (field ebdb-field-role)
			      _style
			      (record ebdb-record-organization))
  (let ((person (ebdb-gethash (slot-value field 'record-uuid) 'uuid))
	(mail (slot-value field 'mail)))
    (if mail
	(format "%s (%s)"
		(ebdb-string person)
		(ebdb-fmt-field fmt mail 'oneline record))
      (ebdb-string person))))

(cl-defmethod ebdb-fmt-field ((fmt ebdb-formatter-ebdb)
			      (field ebdb-field-role)
			      _style
			      (record ebdb-record-person))
  (let ((org (ebdb-gethash (slot-value field 'org-uuid) 'uuid))
	(mail (slot-value field 'mail)))
    (if mail
	(format "%s (%s)"
		(ebdb-string org)
		(ebdb-fmt-field fmt mail 'oneline record))
      (ebdb-string org))))

(defsubst ebdb-indent-string (string column)
  "Indent nonempty lines in STRING to COLUMN (except first line).
This happens in addition to any pre-defined indentation of STRING."
  (replace-regexp-in-string "\n\\([^\n]\\)"
                            (concat "\n" (make-string column ?\s) "\\1")
                            string))

;;; Record display:
;;; This inserts formatted (pieces of) records into the EBDB buffer.

(cl-defmethod ebdb-fmt-record-body ((_fmt ebdb-formatter-ebdb-multiline)
				    (_record ebdb-record)
				    (field-list list))
  (let* ((indent
	  (if field-list
	      (apply #'max (mapcar (lambda (f)
				     (string-width (car f)))
				   field-list))
	    0))
	 (label-fmt (format " %%%ds" indent))
	 ;; `window-text-width' doesn't work for pop-up buffers,
	 ;; they're not displayed yet!  How do we resolve this...?
	 (fill-column (window-text-width))
	 (fill-prefix (make-string (+ 3 indent) ?\s))
	 (paragraph-start "[^:]+:[^\n]+$"))

    (dolist (c field-list)
      (insert (format label-fmt (car c)))
      (put-text-property (line-beginning-position) (point) 'face 'ebdb-label)
      (insert
       (concat
	": "
	;; If I understood the mechanics of filling better, I
	;; could probably do away with `ebdb-indent-string'
	;; altogether.
	(ebdb-indent-string (cdr c) (+ indent 3))))
      ;; If there are newlines in the value string, assume the field
      ;; knows what's it's doing re filling and formatting.
      (unless (or (string-match-p "\n" (cdr c))
		  (null ebdb-fill-field-values))
      	(fill-paragraph))
      (insert "\n"))))

(cl-defmethod ebdb-fmt-record-body ((_fmt ebdb-formatter-ebdb-oneline)
				    (_record ebdb-record)
				    (field-list list))
  (insert " ")
  (insert (mapconcat #'cdr field-list ", ")))

(cl-defmethod ebdb-fmt-record-header ((_fmt ebdb-formatter-ebdb)
				      (record ebdb-record)
				      (field-list list))
  "Insert header for RECORD."
  ;; Name
  (let ((record-class (eieio-object-class-name record))
	(db-chars (ebdb-record-db-char-string record))
	step)
    (when db-chars
      (insert db-chars " "))
    (setq step (point))
    (insert (slot-value (ebdb-record-cache record) 'name-string))
    (add-text-properties (line-beginning-position) (point)
			 (list 'ebdb-record record-class))
    (add-text-properties step (point)
			 (list
			  'face (cdr (assoc record-class ebdb-name-face-alist)))))
  ;; Everything else
  (when field-list
    (insert " - ")
    (insert
     (mapconcat
      (lambda (f)
	;; We need to special-case image field, because it is inserted
	;; differently.  Conveniently, this also allows us to always
	;; keep the image at the end of the header.
	(unless (eql (plist-get f :class) 'ebdb-field-image)
	  (cdr f)))
      field-list
      ", "))
    ;; TODO: Check if image is in field-list, not if it exists!
    (when (and (slot-boundp record 'image)
	       (slot-value record 'image)
	       (display-images-p))
      (let ((image (ebdb-field-image-get (slot-value record 'image) record)))
	(when image
	  (insert " ")
	  (insert-image image))))))

(cl-defmethod ebdb-fmt-record-header :after ((_fmt ebdb-formatter-ebdb-multiline)
					     (_record ebdb-record)
					     _field-list)
  (insert "\n"))

(cl-defmethod ebdb-fmt-record ((fmt ebdb-formatter-ebdb)
			       (record ebdb-record))
  (let ((field-plist
	 (ebdb-fmt-process-fields
	  fmt record
	  (ebdb-fmt-sort-fields
	   fmt record
	   (ebdb-fmt-collect-fields
	    fmt record))))
	(header-classes (cdr (assoc (eieio-object-class-name record)
				     (slot-value fmt 'header))))
	header-fields body-fields)
    (dolist (f field-plist)
      (push (ebdb-fmt-compose-field fmt f record)
	    (if (ebdb-class-in-list-p (plist-get f :class) header-classes)
		header-fields
	      body-fields)))
    (ebdb-fmt-record-header
     fmt
     record
     header-fields)

    (ebdb-fmt-record-body
     fmt
     record
     body-fields)
    (insert "\n")))

(defun ebdb-display-records (records &optional fmt append
                                     select horiz-p)
  "Display RECORDS using FMT.
If APPEND is non-nil append RECORDS to the already displayed records.
Otherwise RECORDS overwrite the displayed records.
SELECT and HORIZ-P have the same meaning as in `ebdb-pop-up-window'."
  (interactive (list (ebdb-completing-read-records "Display records: ")
                     (ebdb-formatter-prefix)))
  (if (ebdb-append-display-p) (setq append t))
  ;; `ebdb-redisplay-record' calls `ebdb-display-records'
  ;; with display information already amended to RECORDS.
  (unless (or (null records)
              (consp (car records)))
    ;; add fmt and a marker to the local list of records
    (setq records (mapcar (lambda (record)
                            (list record fmt (make-marker) nil))
                          records)))

  (let ((first-new (caar records))	; first new record
        new-name)

    ;; If `ebdb-multiple-buffers' is non-nil we create a new EBDB buffer
    ;; when not already within one.  The new buffer name starts with a space,
    ;; i.e. it does not clutter the buffer list.
    (when (and ebdb-multiple-buffers
               (not (assq 'ebdb-buffer-name (buffer-local-variables))))
      (setq new-name (concat " *EBDB " (if (functionp ebdb-multiple-buffers)
                                           (funcall ebdb-multiple-buffers)
                                         (buffer-name))
			     "*"))
      ;; `ebdb-buffer-name' becomes buffer-local in the current buffer
      ;; as well as in the buffer `ebdb-buffer-name'
      (set (make-local-variable 'ebdb-buffer-name) new-name))

    (with-current-buffer (get-buffer-create ebdb-buffer-name) ; *EBDB*
      ;; If we are appending RECORDS to the ones already displayed,
      ;; then first remove any duplicates, and then sort them.
      (if append
          (let ((old-rec (mapcar 'car ebdb-records)))
            (dolist (record records)
              (unless (memq (car record) old-rec)
                (push record ebdb-records)))
            (setq records
                  (sort ebdb-records
                        (lambda (x y) (ebdb-record-lessp (car x) (car y)))))))

      (ebdb-mode)
      ;; Normally `ebdb-records' is the only EBDB-specific buffer-local variable
      ;; in the *EBDB* buffer.  It is intentionally not permanent-local.
      ;; A value of nil indicates that we need to (re)process the records.
      (setq ebdb-records records)
      (if new-name
          (set (make-local-variable 'ebdb-buffer-name) new-name))

      (unless (or ebdb-silent-internal ebdb-silent)
        (message "Formatting EBDB..."))
      (let ((record-number 0)
	    (fmt ebdb-default-multiline-formatter)
            buffer-read-only all-records start)
        (erase-buffer)
        (ebdb-debug (setq all-records (ebdb-records)))
	(insert (ebdb-fmt-header fmt records))
        (dolist (record records)
          (ebdb-debug (unless (memq (car record) all-records)
                        (error "Record %s does not exist" (car record))))
	  (setq start (set-marker (nth 2 record) (point)))
          (ebdb-fmt-record fmt (car record))
	  (put-text-property start (point) 'ebdb-record-number record-number)
	  (setq record-number (1+ record-number)))
	(insert (ebdb-fmt-footer fmt records))
        (run-hooks 'ebdb-display-hook))

      (unless (or ebdb-silent-internal ebdb-silent)
        (message "Formatting EBDB...done."))
      (set-buffer-modified-p nil)

      (ebdb-pop-up-window select horiz-p)
      (if (not first-new)
          (goto-char (point-min))
        ;; Put point on first new record in *EBDB* buffer.
        (goto-char (nth 2 (assq first-new ebdb-records)))
        (set-window-start (get-buffer-window (current-buffer)) (point))))))

(defun ebdb-undisplay-records (&optional all-buffers)
  "Undisplay records in *EBDB* buffer, leaving this buffer empty.
If ALL-BUFFERS is non-nil undisplay records in all EBDB buffers."
  (dolist (buffer (cond (all-buffers (buffer-list))
                        ((let ((buffer (get-buffer ebdb-buffer-name)))
                           (and (buffer-live-p buffer) (list buffer))))))
    (with-current-buffer buffer
      (when (eq major-mode 'ebdb-mode)
        (let (buffer-read-only)
          (erase-buffer))
        (setq ebdb-records nil)
        (set-buffer-modified-p nil)))))

(defun ebdb-redisplay-record (record &optional sort delete-p)
  "Redisplay RECORD in current EBDB buffer.
If SORT is t, usually because RECORD has a new sortkey, re-sort
the displayed records.
If DELETE-P is non-nil RECORD is removed from the EBDB buffer."
  ;; For deletion in the *EBDB* buffer we use the full information
  ;; about the record in the database. Therefore, we need to delete
  ;; the record in the *EBDB* buffer before deleting the record in
  ;; the database.
  (let ((full-record (assq record ebdb-records)))
    (unless full-record
      (error "Record `%s' not displayed" (ebdb-record-name record)))
    (if (and sort (not delete-p))
        ;; FIXME: For records requiring re-sorting it may be more efficient
        ;; to insert these records in their proper location instead of
        ;; re-displaying all records.
        (ebdb-display-records (list record) nil t)
      (let ((marker (nth 2 full-record))
            (end-marker (nth 2 (car (cdr (memq full-record ebdb-records)))))
	    (fmt (nth 1 full-record))
            buffer-read-only record-number)
        ;; If point is inside record, put it at the beginning of the record.
        (if (and (<= marker (point))
                 (< (point) (or end-marker (point-max))))
            (goto-char marker))
        (save-excursion
          (goto-char marker)
          (setq record-number (get-text-property (point) 'ebdb-record-number))
          (unless delete-p
            ;; First insert the reformatted record, then delete the old one,
            ;; so that the marker of this record cannot collapse with the
            ;; marker of the subsequent record
	    (ebdb-fmt-record fmt (car full-record))
	    (put-text-property marker (point) 'ebdb-record-number record-number)
	    (when (eq (nth 3 full-record) 'mark)
	      (add-face-text-property
	       marker
	       (next-property-change marker nil (line-end-position) )
	       'ebdb-marked)))
          (delete-region (point) (or end-marker (point-max)))
          ;; If we deleted a record we need to update the subsequent
          ;; record numbers.
          (when delete-p
            (let* ((markers (append (mapcar (lambda (x) (nth 2 x))
                                            (cdr (memq full-record ebdb-records)))
                                    (list (point-max))))
                   (start (pop markers)))
              (dolist (end markers)
                (put-text-property start end
                                   'ebdb-record-number record-number)
                (setq start end
                      record-number (1+ record-number))))
            (setq ebdb-records (delq full-record ebdb-records)))
          (run-hooks 'ebdb-display-hook))))))

(defun ebdb-redisplay-record-globally (record &optional sort delete-p)
  "Redisplay RECORD in all EBDB buffers.
If SORT is t, usually because RECORD has a new sortkey, re-sort
the displayed records.
If DELETE-P is non-nil RECORD is removed from the EBDB buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (and (eq major-mode 'ebdb-mode)
               (memq record (mapcar 'car ebdb-records)))
          (let ((window (get-buffer-window ebdb-buffer-name)))
            (if window
                (with-selected-window window
                  (ebdb-redisplay-record record sort delete-p))
              (ebdb-redisplay-record record sort delete-p)))))))

(easy-menu-define
  ebdb-menu ebdb-mode-map "EBDB Menu"
  '("EBDB"
    ("Display"
     ["Previous field" ebdb-prev-field t]
     ["Next field" ebdb-next-field t]
     ["Previous record" ebdb-prev-record t]
     ["Next record" ebdb-next-record t]
     "--"
     ["Show all records" ebdb-display-all-records t]
     ["Show current record" ebdb-display-current-record t]
     ["Omit record" ebdb-omit-record t]
     ["Toggle record mark" ebdb-toggle-record-mark t]
     "--"
     ["Toggle layout" ebdb-toggle-records-format t]
     ["Show all fields" ebdb-display-records-completely t])
    ("Searching"
     ["General search" ebdb t]
     ["Search one record" ebdb-display-records t]
     ["Search name" ebdb-search-name t]
     ["Search organization" ebdb-search-organization t]
     ["Search phone" ebdb-search-phone t]
     ["Search address" ebdb-search-address t]
     ["Search mail" ebdb-search-mail t]
     ["Search xfields" ebdb-search-xfields t]
     ["Search changed records" ebdb-search-changed t]
     ["Search duplicates" ebdb-search-duplicates t]
     "--"
     ["Old time stamps" ebdb-timestamp-older t]
     ["New time stamps" ebdb-timestamp-newer t]
     ["Old creation date" ebdb-creation-older t]
     ["New creation date" ebdb-creation-newer t]
     ["Creation date = time stamp" ebdb-creation-no-change t]
     "--"
     ["Append search" ebdb-append-display t]
     ["Invert search" ebdb-search-invert t])
    ("Mail"
     ["Send mail" ebdb-mail t]
     ["Save mail address" ebdb-mail-address t]
     "--"
     ["Add mail alias" ebdb-add-mail-alias t]
     ["(Re-)Build mail aliases" ebdb-mail-aliases t])
    ("Use database"
     ["Prefix: do all records" ebdb-do-all-records t]
     "--"
     ["Send mail" ebdb-mail t]
     ["Dial phone number" ebdb-dial t]
     ["Browse URL" ebdb-browse-url t]
     ["Copy records as kill" ebdb-copy-records-as-kill t]
     ["Copy fields as kill" ebdb-copy-fields-as-kill t]
     ["Follow relation" ebdb-follow-related t]
     "--"
     ["Print records" ebdb-print t])
    ("Manipulate database"
     ["Prefix: do all records" ebdb-do-all-records t]
     "--"
     ["Create new record" ebdb-create t]
     ["Edit current field" ebdb-edit-field t]
     ["Insert new field" ebdb-insert-field t]
     ["Edit some field" ebdb-edit-foo t]
     ["Transpose fields" ebdb-transpose-fields t]
     ["Delete record or field" ebdb-delete-field-or-record t]
     "--"
     ["Sort addresses" ebdb-sort-addresses t]
     ["Sort phones" ebdb-sort-phones t]
     ["Sort xfields" ebdb-sort-xfields t]
     ["Merge records" ebdb-merge-records t]
     ["Sort database" ebdb-sort-records t]
     ["Delete duplicate mails" ebdb-delete-redundant-mails t]
     "--"
     ["Save EBDB" ebdb-save t]
     ["Revert EBDB" revert-buffer t])
    ("Help"
     ["Brief help" ebdb-help t]
     ["EBDB Manual" ebdb-info t])
    "--"
    ["Quit" quit-window t]))

(defvar ebdb-completing-read-mails-map
  (let ((map (copy-keymap minibuffer-local-completion-map)))
    (define-key map " " 'self-insert-command)
    (define-key map "\t" 'ebdb-complete-mail)
    (define-key map "\M-\t" 'ebdb-complete-mail)
    map)
  "Keymap used by `ebdb-completing-read-mails'.")

;;; window configuration hackery
(defun ebdb-pop-up-window (&optional select horiz-p)
  "Display *EBDB* buffer by popping up a new window.
Finds the largest window on the screen, splits it, displaying the
*EBDB* buffer in the bottom `ebdb-pop-up-window-size' lines (unless
the *EBDB* buffer is already visible, in which case do nothing.)
Select this window if SELECT is non-nil.

If `ebdb-mua-pop-up' is 'horiz, and the first window matching
the predicate HORIZ-P is wider than the car of `ebdb-horiz-pop-up-window-size'
then the window will be split horizontally rather than vertically."
  (let ((buffer (get-buffer ebdb-buffer-name)))
    (unless buffer
      (error "No %s buffer to display" ebdb-buffer-name))
    (cond ((let ((window (get-buffer-window buffer t)))
             ;; We already have a EBDB window so that at most we select it
             (and window
                  (or (not select) (select-window window)))))

          ;; try horizontal split
          ((and horiz-p
                (>= (frame-width) (car ebdb-horiz-pop-up-window-size))
                (let ((window-list (window-list))
                      (b-width (cdr ebdb-horiz-pop-up-window-size))
                      (search t) s-window)
                  (while (and (setq s-window (pop window-list))
                              (setq search (not (funcall horiz-p s-window)))))
                  (unless (or search (<= (window-width s-window)
                                         (car ebdb-horiz-pop-up-window-size)))
                    (condition-case nil ; `split-window' might fail
                        (let ((window (split-window
                                       s-window
                                       (if (integerp b-width)
                                           (- (window-width s-window) b-width)
                                         (round (* (- 1 b-width) (window-width s-window))))
                                       t))) ; horizontal split
                          (set-window-buffer window buffer)
                          (cond (ebdb-dedicated-window
                                 (set-window-dedicated-p window ebdb-dedicated-window))
                                ((fboundp 'display-buffer-record-window) ; GNU Emacs >= 24.1
                                 (set-window-prev-buffers window nil)
                                 (display-buffer-record-window 'window window buffer)))
                          (if select (select-window window))
                          t)
                      (error nil))))))

          ((eq t ebdb-pop-up-window-size)
           (ebdb-pop-up-window-simple buffer select))

          (t ;; vertical split
           (let* ((window (selected-window))
                  (window-height (window-height window)))
             ;; find the tallest window...
             (mapc (lambda (w)
                     (let ((w-height (window-height w)))
                       (if (> w-height window-height)
                           (setq window w window-height w-height))))
                   (window-list))
             (condition-case nil
                 (progn
                   (unless (eql ebdb-pop-up-window-size 1.0)
                     (setq window (split-window ; might fail
                                   window
                                   (if (integerp ebdb-pop-up-window-size)
                                       (- window-height 1 ; for mode line
                                          (max window-min-height ebdb-pop-up-window-size))
                                     (round (* (- 1 ebdb-pop-up-window-size)
                                               window-height))))))
                   (set-window-buffer window buffer) ; might fail
                   (cond (ebdb-dedicated-window
                          (set-window-dedicated-p window ebdb-dedicated-window))
                         ((and (fboundp 'display-buffer-record-window) ; GNU Emacs >= 24.1
                               (not (eql ebdb-pop-up-window-size 1.0)))
                          (set-window-prev-buffers window nil)
                          (display-buffer-record-window 'window window buffer)))
                   (if select (select-window window)))
               (error (ebdb-pop-up-window-simple buffer select))))))))

(defun ebdb-pop-up-window-simple (buffer select)
  "Display BUFFER in some window, selecting it if SELECT is non-nil.
If `ebdb-dedicated-window' is non-nil, mark the window as dedicated."
  (let ((window (if select
                    (progn (pop-to-buffer buffer)
                           (get-buffer-window))
                  (display-buffer buffer))))
    (if ebdb-dedicated-window
        (set-window-dedicated-p window ebdb-dedicated-window))))


;;; EBDB mode

;;;###autoload
(define-derived-mode ebdb-mode special-mode "EBDB"
  "Major mode for viewing and editing the Insidious Big Brother Database.
Letters no longer insert themselves.  Numbers are prefix arguments.
You can move around using the usual cursor motion commands.
\\<ebdb-mode-map>
\\[ebdb-add-mail-alias]\t Add new mail alias to visible records or \
remove it.
\\[ebdb-edit-field]\t Edit the field on the current line.
\\[ebdb-delete-field-or-record]\t Delete the field on the \
current line.  If the current line is the\n\t first line of a record, then \
delete the entire record.
\\[ebdb-insert-field]\t Insert a new field into the current record.  \
Note that this\n\t will let you add new fields of your own as well.
\\[ebdb-transpose-fields]\t Swap the field on the current line with the \
previous field.
\\[ebdb-dial]\t Dial the current phone field.
\\[ebdb-next-record], \\[ebdb-prev-record]\t Move to the next or the previous \
displayed record, respectively.
\\[ebdb-create]\t Create a new record.
\\[ebdb-toggle-records-format]\t Toggle whether the current record is displayed in a \
one-line\n\t listing, or a full multi-line listing.
\\[ebdb-do-all-records]\\[ebdb-toggle-records-format]\t Do that \
for all displayed records.
\\[ebdb-merge-records]\t Merge the contents of the current record with \
some other, and then\n\t delete the current record.
\\[ebdb-omit-record]\t Remove the current record from the display without \
deleting it from\n\t the database.  This is often a useful thing to do \
before using one\n\t of the `*' commands.
\\[ebdb]\t Search for records in the database (on all fields).
\\[ebdb-search-mail]\t Search for records by mail address.
\\[ebdb-search-organization]\t Search for records by organization.
\\[ebdb-search-xfields]\t Search for records by xfields.
\\[ebdb-search-name]\t Search for records by name.
\\[ebdb-search-changed]\t Display records that have changed since the database \
was saved.
\\[ebdb-mail]\t Compose mail to the person represented by the \
current record.
\\[ebdb-do-all-records]\\[ebdb-mail]\t Compose mail \
to everyone whose record is displayed.
\\[ebdb-save]\t Save the EBDB file to disk.
\\[ebdb-print]\t Create a TeX file containing a pretty-printed version \
of all the\n\t records in the database.
\\[ebdb-do-all-records]\\[ebdb-print]\t Do that for the \
displayed records only.
\\[other-window]\t Move to another window.
\\[ebdb-info]\t Read the Info documentation for EBDB.
\\[ebdb-help]\t Display a one line command summary in the echo area.
\\[ebdb-browse-url]\t Visit Web sites listed in the `url' field(s) of the current \
record.

For address completion using the names and mail addresses in the database:
\t in Sendmail mode, type \\<mail-mode-map>\\[ebdb-complete-mail].
\t in Message mode, type \\<message-mode-map>\\[ebdb-complete-mail].

Important variables:
\t `ebdb-auto-revert'
\t `ebdb-ignore-redundant-mails'
\t `ebdb-case-fold-search'
\t `ebdb-completion-list'
\t `ebdb-default-domain'
\t `ebdb-layout'
\t `ebdb-file'
\t `ebdb-check-auto-save-file'
\t `ebdb-pop-up-layout'
\t `ebdb-pop-up-window-size'
\t `ebdb-add-name'
\t `ebdb-add-aka'
\t `ebdb-add-mails'
\t `ebdb-new-mails-primary'
\t `ebdb-read-only'
\t `ebdb-mua-pop-up'
\t `ebdb-user-mail-address-re'

There are numerous hooks.  M-x apropos ^ebdb.*hook RET

\\{ebdb-mode-map}"
  (setq truncate-lines t
        mode-line-buffer-identification
        (list 24 (buffer-name) "  "
              '(:eval (format "%d/%d/%d"
                              (1+ (or (get-text-property
                                       (point) 'ebdb-record-number) -1))
                              (length ebdb-records)
                              (length (ebdb-records))))
              '(:eval (concat "  "
                              (ebdb-concat " " (elt ebdb-modeline-info 0)
                                           (elt ebdb-modeline-info 2)))))
        mode-line-modified
        ;; For the mode-line we want to be fast. So we skip the checks
        ;; performed by `ebdb-with-db-buffer'.
        '(:eval (if (object-assoc t 'dirty ebdb-db-list) "**" "--")))
  ;; `ebdb-revert-buffer' acts on `ebdb-buffer'.  Yet this command is usually
  ;; called from the *EBDB* buffer.
  ;; (set (make-local-variable 'revert-buffer-function)
  ;;      'ebdb-revert-buffer)
  (add-hook 'post-command-hook 'force-mode-line-update nil t))



(defun ebdb-sendmail-menu (record)
  "Menu items for email addresses of RECORD."
  (let ((mails (ebdb-record-mail record)))
    (list
     (if (cdr mails)
         ;; Submenu for multiple mail addresses
         (cons "Send mail to..."
               (mapcar (lambda (address)
                         (vector address `(ebdb-compose-mail
                                           ,(ebdb-dwim-mail record address))
                                 t))
                       mails))
       ;; Single entry for single mail address
       (vector (concat "Send mail to " (car mails))
               `(ebdb-compose-mail ,(ebdb-dwim-mail record (car mails)))
               t)))))

;; (defun ebdb-field-menu (record field)
;;   "Menu items specifically for FIELD of RECORD."
;;   (let ((type (car field)))
;;     (append
;;      (list
;;       (format "Commands for %s Field:"
;;               (cond ((eq type 'xfields)
;;                      (format "\"%s\"" (symbol-name (car (nth 1 field)))))
;;                     ((eq type 'name) "Name")
;;                     ((eq type 'affix) "Affix")
;;                     ((eq type 'organization) "Organization")
;;                     ((eq type 'aka) "Alternate Names")
;;                     ((eq type 'mail) "Mail Addresses")
;;                     ((memq type '(address phone))
;;                      (format "\"%s\" %s" (aref (nth 1 field) 0)
;;                              (capitalize (symbol-name type)))))))
;;      (cond ((eq type 'phone)
;;             (list (vector (concat "Dial " (ebdb-string (nth 1 field)))
;;                           `(ebdb-dial ',field nil) t)))
;;            ((eq type 'xfields)
;;             (let* ((field (cadr field))
;;                    (type (car field)))
;;               (cond ((eq type 'url )
;;                      (list (vector (format "Browse \"%s\"" (cdr field))
;;                                    `(ebdb-browse-url ,record) t)))))))
;;      '(["Edit Field" ebdb-edit-field t])
;;      (unless (eq type 'name)
;;        '(["Delete Field" ebdb-delete-field-or-record t])))))

(defun ebdb-insert-field-menu (record)
  "Submenu for inserting a new field for RECORD."
  (cons "Insert New Field..."
        (mapcar
         (lambda (field)
           (if (stringp field) field
             (vector (symbol-name field)
                     `(ebdb-insert-field
                       ,record ',field (ebdb-read-field ,record ',field
                                                        ,current-prefix-arg))
                     (not (or (and (eq field 'organization)
                                   (ebdb-record-organizations record))
                              (and (eq field 'mail) (ebdb-record-mail record))
                              (and (eq field 'aka) (ebdb-record-aka record))
                              (assq field (ebdb-record-user-fields record)))))))
         (append '(affix organization aka phone address mail)
                 '("--") ebdb-user-label-list))))

(defun ebdb-mouse-menu (event)
  "EBDB mouse menu for EVENT,"
  (interactive "e")
  (mouse-set-point event)
  (let* ((record (ebdb-current-record))
         (field  (ebdb-current-field))
         (menu (if (and record field (functionp ebdb-user-menu-commands))
                   (funcall ebdb-user-menu-commands record field)
                 ebdb-user-menu-commands)))
    (if record
        (popup-menu
         (append
          (list
           (format "Commands for record \"%s\":" (ebdb-record-name record))
           ["Delete Record" ebdb-delete-records t]
           ["Toggle Record Display Layout" ebdb-toggle-records-format t]
           ["Fully Display Record" ebdb-display-records-completely t]
           ["Omit Record" ebdb-omit-record t]
           ["Merge Record" ebdb-merge-records t])
          (if (ebdb-record-mail record)
              (ebdb-sendmail-menu record))
          (list "--" (ebdb-insert-field-menu record))
          (if field
              (cons "--" (ebdb-field-menu record field)))
          (if menu
              (append '("--" "User Defined Commands") menu)))))))

(defun ebdb-scan-property (property predicate n)
  "Scan for change of PROPERTY matching PREDICATE for N times.
Return position of beginning of matching interval."
  (let ((fun (if (< 0 n) 'next-single-property-change
               'previous-single-property-change))
        (limit (if (< 0 n) (point-max) (point-min)))
        (nn (abs n))
        (i 0)
        (opoint (point))
        npoint)
    ;; For backward search, move point to beginning of interval with PROPERTY.
    (if (and (<= n 0)
             (< (point-min) opoint)
             (let ((prop (get-text-property opoint property)))
               (and (eq prop (get-text-property (1- opoint) property))
                    (funcall predicate prop))))
        (setq opoint (previous-single-property-change opoint property nil limit)))
    (if (zerop n)
        opoint ; Return beginning of interval point is in
      (while (and (< i nn)
                  (let (done)
                    (while (and (not done)
                                (setq npoint (funcall fun opoint property nil limit)))
                      (cond ((and (/= opoint npoint)
                                  (funcall predicate (get-text-property
                                                      npoint property)))
                             (setq opoint npoint done t))
                            ((= opoint npoint)
                             ;; Search reached beg or end of buffer: abort.
                             (setq done t i nn npoint nil))
                            (t (setq opoint npoint))))
                    done))
        (setq i (1+ i)))
      npoint)))

(defun ebdb-next-record (n)
  "Move point to the beginning of the next EBDB record.
With prefix N move forward N records."
  (interactive "p")
  (let ((npoint (ebdb-scan-property 'ebdb-record-number 'integerp n)))
    (if npoint (goto-char npoint)
      (error "No %s record" (if (< 0 n) "next" "previous")))))

(defun ebdb-prev-record (n)
  "Move point to the beginning of the previous EBDB record.
With prefix N move backwards N records."
  (interactive "p")
  (ebdb-next-record (- n)))

(defun ebdb-next-field (n)
  "Move point to next (sub)field.
With prefix N move forward N (sub)fields."
  (interactive "p")
  (let ((npoint (ebdb-scan-property
                 'ebdb-field
		 ;; TODO: This is unnecessary.
                 #'identity
                 n)))
    (if npoint (goto-char npoint)
      (error "No %s field" (if (< 0 n) "next" "previous")))))

(defun ebdb-prev-field (n)
  "Move point to previous (sub)field.
With prefix N move backwards N (sub)fields."
  (interactive "p")
  (ebdb-next-field (- n)))

(defun ebdb-follow-related (record field)
  "If point is on a role or relationship field, display the
  related record."
  (interactive (list (ebdb-current-record)
		     (ebdb-current-field)))
  (let ((related (ebdb-record-related record field)))
    (if related
	(ebdb-display-records (cons related
				    (mapcar #'car ebdb-records))
			      ebdb-default-multiline-formatter
			      t)
      (message "Field %s provides no relationships"
	       (ebdb-field-readable-name field)))))

(defun ebdb-toggle-record-mark (record &optional mark)
  "Mark or unmark RECORD."
  (interactive
   (list (ebdb-current-record t)
	 'mark))
  (setf (nth 3 record) (if (nth 3 record) nil mark))
  (ebdb-redisplay-record (car record)))

(defun ebdb-toggle-all-record-marks ()
  "Reverse the marked status of all records."
  (interactive)
  (mapcar
   (lambda (rec)
     (ebdb-toggle-record-mark rec 'mark))
   ebdb-records))


;; clean-up functions

;;; Sometimes one gets mail from foo@bar.baz.com, and then later gets mail
;;; from foo@baz.com.  At this point, one would like to delete the bar.baz.com
;;; address, since the baz.com address is obviously superior.

(defun ebdb-mail-redundant-re (mail)
  "Return a regexp matching redundant variants of email address MAIL.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\".
Return nil if MAIL is not a valid plain email address.
In particular, ignore addresses \"Joe Smith <foo@baz.com>\"."
  (let* ((match (string-match "\\`\\([^ ]+\\)@\\(.+\\)\\'" mail))
         (name (and match (match-string 1 mail)))
         (host (and match (match-string 2 mail))))
    (if (and name host)
        (concat (regexp-quote name) "@.*\\." (regexp-quote host)))))

(defun ebdb-delete-redundant-mails (records &optional query update)
  "Delete redundant or duplicate mails from RECORDS.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\".
Duplicates may (but should not) occur if we feed EBDB automatically.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
If QUERY is non-nil (as in interactive calls, unless we use a prefix arg)
query before deleting the redundant mail addresses.
If UPDATE is non-nil (as in interactive calls) update the database.
Otherwise, this is the caller's responsiblity.

Noninteractively, this may be used as an element of `ebdb-notice-record-hook'
or `ebdb-change-hook'.  However, see also `ebdb-ignore-redundant-mails',
which is probably more suited for your needs."
  (interactive (list (ebdb-do-records) (not current-prefix-arg) t))
  (dolist (record (ebdb-record-list records))
    (let (mails redundant okay)
      ;; We do not look at the canonicalized mail addresses of RECORD.
      ;; An address "Joe Smith <foo@baz.com>" can only be entered manually
      ;; into EBDB, and we assume that this is what the user wants.
      ;; Anyway, if a mail field contains all the elements
      ;; foo@baz.com, "Joe Smith <foo@baz.com>", "Jonathan Smith <foo@baz.com>"
      ;; we do not know which address to keep and which ones to throw.
      (dolist (mail (ebdb-record-mail record))
        (if (assoc-string mail mails t) ; duplicate mail address
            (push mail redundant)
          (push mail mails)))
      (let ((mail-re (delq nil (mapcar 'ebdb-mail-redundant-re mails)))
            (case-fold-search t))
        (if (not (cdr mail-re)) ; at most one mail-re address to consider
            (setq okay (nreverse mails))
          (setq mail-re (concat "\\`\\(?:" (mapconcat 'identity mail-re "\\|")
                                "\\)\\'"))
          (dolist (mail mails)
            (if (string-match mail-re mail) ; redundant mail address
                (push mail redundant)
              (push mail okay)))))
      (let ((form (format "redundant mail%s %s"
                          (if (< 1 (length redundant)) "s" "")
                          (ebdb-concat 'mail (nreverse redundant)))))
        (when (and redundant
                   (or (not query)
                       (y-or-n-p (format "Delete %s: " form))))
          (unless query (message "Deleting %s" form))
          (ebdb-record-set-field record 'mail okay)
          (when update
            (ebdb-change-record record)))))))

(defun ebdb-search-duplicates (&optional fields)
  "Search all records that have duplicate entries for FIELDS.
The list FIELDS may contain the symbols `name', `mail', and `aka'.
If FIELDS is nil use all these fields.  With prefix, query for FIELDS.
The search results are displayed in the EBDB buffer."
  (interactive (list (if current-prefix-arg
                         (list (intern (completing-read "Field: "
                                                        '("name" "mail" "aka")
                                                        nil t))))))
  (setq fields (or fields '(name mail aka)))
  (let (hash ret)
    (dolist (record (ebdb-records))

      (when (and (memq 'name fields)
                 (ebdb-record-name record)
                 (setq hash (ebdb-gethash (ebdb-record-name record)
                                          '(fl-name lf-name aka)))
                 (> (length hash) 1))
        (setq ret (append hash ret))
        (message "EBDB record `%s' has duplicate name."
                 (ebdb-record-name record))
        (sit-for 0))

      (if (memq 'mail fields)
          (dolist (mail (ebdb-record-mail-canon record))
              (setq hash (ebdb-gethash mail '(mail)))
              (when (> (length hash) 1)
                (setq ret (append hash ret))
                (message "EBDB record `%s' has duplicate mail `%s'."
                         (ebdb-record-name record) mail)
                (sit-for 0))))

      (if (memq 'aka fields)
          (dolist (aka (ebdb-record-aka record))
            (setq hash (ebdb-gethash aka '(fl-name lf-name aka)))
            (when (> (length hash) 1)
              (setq ret (append hash ret))
              (message "EBDB record `%s' has duplicate aka `%s'"
                       (ebdb-record-name record) aka)
              (sit-for 0)))))

    (ebdb-display-records (sort (delete-dups ret)
                                'ebdb-record-lessp))))

(defun ebdb-touch-records (records)
  "Touch RECORDS by calling `ebdb-change-hook' unconditionally."
  (interactive (list (ebdb-do-records)))
  (dolist (record (ebdb-record-list records))
    (setf (slot-value record 'dirty) t)))

;;; Time-based functions

(defmacro ebdb-compare-records (cmpval label compare)
  "Builds a lambda comparison function that takes one argument, RECORD.
RECORD is returned if (COMPARE VALUE CMPVAL) is t, where VALUE
is the value of xfield LABEL of RECORD."
  `(lambda (record)
     (let ((val (ebdb-record-field record ,label)))
       (if (and val (,compare val ,cmpval))
           record))))

(defsubst ebdb-string> (a b)
  (not (or (string= a b)
           (string< a b))))

;;;###autoload
(defun ebdb-timestamp-older (date &optional fmt)
  "Display records with timestamp older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Older than date (yyyy-mm-dd): ")
                     (ebdb-formatter-prefix)))
  (ebdb-search-prog (ebdb-compare-records date 'timestamp 'time-less-p) fmt))

;;;###autoload
(defun ebdb-timestamp-newer (date &optional fmt)
  "Display records with timestamp newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Newer than date (yyyy-mm-dd): ")
                     (ebdb-formatter-prefix)))
  (ebdb-search-prog (ebdb-compare-records date 'timestamp
					  (lambda (l r) (null (time-less-p l r))))
		    fmt))

;;;###autoload
(defun ebdb-creation-older (date &optional fmt)
  "Display records with creation-date older than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Older than date (yyyy-mm-dd): ")
                     (ebdb-formatter-prefix)))
  (ebdb-search-prog (ebdb-compare-records date 'creation-date string<) fmt))

;;;###autoload
(defun ebdb-creation-newer (date &optional fmt)
  "Display records with creation-date newer than DATE.
DATE must be in yyyy-mm-dd format."
  (interactive (list (read-string "Newer than date (yyyy-mm-dd): ")
                     (ebdb-formatter-prefix)))
  (ebdb-search-prog (ebdb-compare-records date 'creation-date ebdb-string>) fmt))

;;;###autoload
(defun ebdb-creation-no-change (&optional fmt)
  "Display records that have the same timestamp and creation-date."
  (interactive (list (ebdb-formatter-prefix)))
  (ebdb-search-prog
   ;; RECORD is bound in `ebdb-search-prog'.
   (ebdb-compare-records (ebdb-record-field record 'timestamp)
                         'creation-date 'equal) fmt))

;;; Parsing phone numbers
;;; XXX this needs expansion to handle international prefixes properly
;;; i.e. +353-number without discarding the +353 part. Problem being
;;; that this will necessitate yet another change in the database
;;; format for people who are using north american numbers.

(defsubst ebdb-subint (string num)
  "Used for parsing phone numbers."
  (string-to-number (match-string num string)))

(defun ebdb-message-search (name mail)
  "Return list of EBDB records matching NAME and/or MAIL.
First try to find a record matching both NAME and MAIL.
If this fails try to find a record matching MAIL.
If this fails try to find a record matching NAME.
NAME may match FIRST_LAST, LAST_FIRST or AKA.

This function performs a fast search using `ebdb-hashtable'.
NAME and MAIL must be strings or nil.
See `ebdb-search' for searching records with regexps."
  (when (or name mail)
    (unless ebdb-db-list
      (ebdb-load))
    (let ((mrecords (if mail (ebdb-gethash mail '(mail))))
          (nrecords (if name (ebdb-gethash name '(fl-name lf-name aka)))))
      ;; (1) records matching NAME and MAIL
      (or (and mrecords nrecords
               (let (records)
                 (dolist (record nrecords)
                   (mapc (lambda (mr) (if (and (eq record mr)
                                               (not (memq record records)))
                                          (push record records)))
                         mrecords))
                 records))
          ;; (2) records matching MAIL
          mrecords
          ;; (3) records matching NAME
          nrecords))))

(defmacro ebdb-with-record-edits (spec &rest body)
  "Run BODY on all records listed in the cdr of SPEC.

This macro checks that each record is editable; ie, that it
doesn't belong to a read-only database. It also throws an error
and bails out if any of the database are unsynced.

Then bind each editable record to the car of SPEC in turn, run
`ebdb-change-hook' on the record, excecute BODY, run
`ebdb-after-change-hook', and redisplay the record.

SPEC should look like the first argument to `dolist'.  This macro
should be called as:

\(ebdb-with-record-edits (r record-list)
  ...\)

Note that RECORD-LIST will be replaced with the list of
actually-editable records."
  (declare (indent 1) (debug ((symbolp form) body)))
  (let ((editable-records (cl-gensym))
	(bad-dbs (cl-gensym))
	(good-dbs (cl-gensym))
	(fields-from-head (cl-gensym)))
    `(let ((,fields-from-head 0)
	   ,editable-records ,bad-dbs ,good-dbs)
       ;; This is a trick for keeping point from jumping all over the
       ;; place when fields are added/deleted/edited.  We record how
       ;; many fields away from the record header point is, and after
       ;; the editing process we go to that number again.
       (save-excursion
	 (while (null (get-text-property (line-beginning-position) 'ebdb-record))
	   (condition-case nil
	       (ebdb-prev-field 1)
	     (error (forward-line -1)))
	   (cl-incf ,fields-from-head)))
       (dolist (r ,(nth 1 spec))
	 (unless
	     ;; "Unless the record has a bum database..."
	     (catch 'bad
	       ;; Return nil unless we throw a 'bad.
	       (dolist (d (slot-value (ebdb-record-cache r) 'database) nil)
		 (cond ((object-assoc (slot-value d 'file) 'file ,good-dbs))
		       ((object-assoc (slot-value d 'file) 'file ,bad-dbs)
			(throw 'bad t))
		       (t
			(if (ebdb-db-editable d t t)
			    (push d ,good-dbs)
			  (push d ,bad-dbs)
			  (throw 'bad t))))))
	   ;; No bum database, it's okay.
	   (push r ,editable-records)))
       (dolist (,(car spec) ,editable-records)
	 (run-hook-with-args 'ebdb-change-hook ,(car spec))
	 ,@body
	 (run-hook-with-args 'ebdb-after-change-hook ,(car spec))
	 (ebdb-redisplay-record-globally ,(car spec)))
       (dotimes (_i ,fields-from-head)
	 (ebdb-next-field 1)))))

;;;###autoload
(defun ebdb-create-record (db record-class)
  "Create a new EBDB record.

With no prefix argument, assume that we're creating a record in
the first database found in `ebdb-db-list', using its default
record class.

With a prefix arg, prompt for the database to use (assuming there
is more than one), and prompt for the record class to use."
  (interactive
   (list (car ebdb-db-list)
	 (slot-value db 'record-class)))
  (let ((record (ebdb-read record-class)))
   (condition-case nil
       (progn
	 (ebdb-db-editable db nil t)
	 (run-hook-with-args 'ebdb-create-hook record)
	 (run-hook-with-args 'ebdb-change-hook record)
	 (ebdb-db-add-record db record)
	 (ebdb-init record)
	 (run-hook-with-args 'ebdb-after-change-hook record)
	 (ebdb-display-records (list record)))
     (ebdb-readonly-db
      (message "%s is read-only" (ebdb-string db)))
     (ebdb-unsynced-db
      (message "%s is out of sync" (ebdb-string db))))))

;;;###autoload
(defun ebdb-create-record-extended ()
  (interactive)
  (let ((db
	 (if (or (= 1 (length ebdb-db-list))
		 (null arg))
	     (car ebdb-db-list)
	   (ebdb-prompt-for-db)))
	(record-class
	 (eieio-read-subclass "Use which record class? " 'ebdb-record nil t)))
    (ebdb-create-record db record-class)))

;;;###autoload
(defun ebdb-insert-field (records)
  (interactive
   (list (ebdb-do-records)))
  (pcase-let
      ((`(,label (,_slot . ,class))
	(ebdb-prompt-for-field-type
	 (ebdb-record-field-slot-query
	  (eieio-object-class (car records))))))
    (let
	((field (ebdb-read class
			   (when (equal class 'ebdb-field-user-simple)
			     `(:object-name ,label))))
	 new-slot)
      (ebdb-with-record-edits (r records)
	;; If we're adding the same field to many different records, of
	;; different classes, it's possible that some of the records
	;; won't accept this field, or will accept it in a different
	;; slot.
	(condition-case nil
	    (progn
	      (setq new-slot (car (ebdb-record-field-slot-query
				   (eieio-object-class r) `(nil . ,class))))
	      (ebdb-record-insert-field r new-slot field))
	  (ebdb-unacceptable-field
	   (message "Record %s cannot accept field %s" (ebdb-string r) field)
	   (sit-for 2)))))))

;; TODO: Allow editing of multiple record fields simultaneously.
;;;###autoload
(defun ebdb-edit-field (record field)
  "Edit the field under point.  If point is on the name header of
the record, change the name of the record."
  (interactive
   (save-excursion
     (let* ((field (or (get-text-property (point) 'ebdb-record)
		       (ebdb-current-field)
		       (user-error "Point not in a field"))))
       (list (ebdb-current-record)
	     field))))
  (ebdb-with-record-edits (r (list record))
    (if (eieio-object-p field)
	(ebdb-record-change-field record field)
      (ebdb-record-change-name record))))

;;;###autoload
(defun ebdb-edit-foo (record field)
  "For RECORD edit some FIELD (mostly interactively).

Interactively, if called without a prefix, edit the notes field
of RECORD.  When called with a prefix, prompt the user for a
field to edit."
  (interactive
   (let* ((record (ebdb-current-record))
          field field-list)
     (if current-prefix-arg
	 (setq field-list
	       (mapcar
		(lambda (f)
		  (let ((field (cdr f)))
		    (cons (concat
			   (ebdb-field-readable-name field)
			   (when (slot-exists-p field 'object-name)
			     (format " (%s)" (slot-value field 'object-name)))
			   " "
			   (car (split-string (ebdb-string field) "\n")))
			  (cdr f))))
		(ebdb-record-current-fields record))
	       field
	       (cdr
		(assoc
		 (completing-read
		  "Field: "
		  field-list)
		 field-list)))
       (setq field (ebdb-record-field record 'notes)))
     (list record field)))
  (ebdb-with-record-edits (r (list record))
    (if field
	(ebdb-record-change-field record field)
      ;; This is wrong, we need to rework `ebdb-insert-field' so we can
      ;; call it with these arguments.  Shouldn't be doing low-level
      ;; work here.
      (setq field (ebdb-read ebdb-default-notes-class))
      (ebdb-record-insert-field record 'notes field))))

;; (ebdb-list-transpose '(a b c d) 1 3)
(defun ebdb-list-transpose (list i j)
  "For LIST transpose elements I and J destructively.
I and J start with zero.  Return the modified LIST."
  (if (eq i j)
      list ; ignore that i, j could be invalid
    (let (a b c)
      ;; Travel down LIST only once
      (if (> i j) (setq a i i j j a)); swap
      (setq a (nthcdr i list)
            b (nthcdr (- j i) a)
            c (car b))
      (unless b (error "Args %i, %i beyond length of list." i j))
      (setcar b (car a))
      (setcar a c)
      list)))

;;;###autoload
(defun ebdb-delete-field-or-record (records field &optional noprompt)
  "For RECORDS delete FIELD.
If FIELD is the `name' field, delete RECORDS from datanbase.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records',
and FIELD is the field point is on.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  ;; The value of FIELD is whatever `ebdb-current-field' returns.
  ;; This way we can identify more accurately what really needs
  ;; to be done.
  (interactive
   (list (ebdb-do-records) (ebdb-current-field) current-prefix-arg))
  (unless field (error "Not a field"))
  (setq records (ebdb-record-list records))
  ;; TODO: Will this delete the record if we try to delete an AKA?  We
  ;; need a less ambiguous way of knowing if the record should be
  ;; deleted.  Probably `ebdb-fmt-record-header' should lay some
  ;; special text property on the header name, and we should check for
  ;; that.
  (if (get-text-property (point) 'ebdb-record)
      (ebdb-delete-records records noprompt)
    (ebdb-with-record-edits (record records)
      (when (or noprompt
		(y-or-n-p (format "Delete this `%s' field (of %s)? "
				  (ebdb-field-readable-name field)
				  (ebdb-record-name record))))
	(ebdb-record-delete-field
	 record (car (ebdb-record-field-slot-query
		      (eieio-object-class record)
		      (cons nil (eieio-object-class field))))
	 field))
      (ebdb-redisplay-record-globally record))))

;;;###autoload
(defun ebdb-delete-records (records &optional noprompt)
  "Delete RECORDS.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
If prefix NOPROMPT is non-nil, do not confirm deletion."
  (interactive (list (ebdb-do-records) current-prefix-arg))
  (dolist (record (ebdb-record-list records))
    (when (or noprompt
              (y-or-n-p (format "Delete the EBDB record of %s? "
                                (ebdb-string record))))
      (ebdb-delete record)
      (ebdb-redisplay-record-globally record nil t))))

;;;###autoload
(defun ebdb-move-records (records db)
  (interactive (list (ebdb-do-records)
		     (ebdb-prompt-for-db)))
  (ebdb-with-record-edits (r records)
    (ebdb-move-record r db)))

;;;###autoload
(defun ebdb-copy-records (records db)
  (interactive (list (ebdb-do-records)
		     (ebdb-prompt-for-db)))
  (ebdb-with-record-edits (r records)
    (ebdb-copy-record r db)))

;;;###autoload
(defun ebdb-display-all-records (&optional fmt)
  "Show all records.
If invoked in a *EBDB* buffer point stays on the currently visible record.
Inverse of `ebdb-display-current-record'."
  (interactive (list (ebdb-formatter-prefix)))
  (let ((current (ignore-errors (ebdb-current-record))))
    (ebdb-display-records (ebdb-records) fmt)
    (when (setq current (assq current ebdb-records))
      (redisplay) ; Strange display bug??
      (goto-char (nth 2 current)))))
      ;; (set-window-point (selected-window) (nth 2 current)))))

;;;###autoload
(defun ebdb-display-current-record (&optional fmt)
  "Narrow to current record.  Inverse of `ebdb-display-all-records'."
  (interactive (list (ebdb-formatter-prefix)))
  (ebdb-display-records (list (ebdb-current-record)) fmt))

(defun ebdb-change-records-fmt (records fmt)
  (dolist (record records)
    (unless (eq fmt (nth 1 record))
      (setf (nth 1 record) fmt)
      (ebdb-redisplay-record (car record)))))

;;;###autoload
(defun ebdb-toggle-records-format (records &optional arg)
  "Toggle fmt of RECORDS (elided or expanded).
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
With prefix ARG 0, RECORDS are displayed elided.
With any other non-nil ARG, RECORDS are displayed expanded."
  (interactive (list (ebdb-do-records t) current-prefix-arg))
  (let* ((current-fmt (nth 1 (assq (ebdb-current-record) ebdb-records)))
	 (formatters (ebdb-available-ebdb-formatters))
         (fmt
          (cond ((eq arg 0)
                 ebdb-default-oneline-formatter)
                ((or (null current-fmt)
		     (null (memq current-fmt formatters)))
                 ebdb-default-multiline-formatter)
		;; layout is not the last element of layout-alist
		;; and we switch to the following element of layout-alist
                ((let ((idx (cl-position current-fmt formatters)))
		   (if (= (1+ idx) (length formatters))
		       (car formatters)
		     (nth (1+ idx) formatters)))))))
    (message "Using %S layout" (ebdb-string fmt))
    (ebdb-change-records-fmt (ebdb-record-list records t) fmt)))

;;;###autoload
(defun ebdb-display-records-completely (records)
  "Display RECORDS using layout `full-multi-line' (i.e., display all fields).
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'."
  (interactive (list (ebdb-do-records t)))
  (let* ((record (ebdb-current-record))
         (current-fmt (nth 1 (assq record ebdb-records)))
	 ;; TODO: Something weird happens with duplication of
	 ;; formatter objects when we do this.
         (fmt (clone current-fmt :include nil :exclude nil)))
    (ebdb-change-records-fmt (ebdb-record-list records t) fmt)))

;;;###autoload
(defun ebdb-display-records-with-fmt (records fmt)
  "Display RECORDS using FMT. "
  (interactive
   (list (ebdb-do-records t)
         (cdr (assoc-string
	       (completing-read "Format: "
				(mapcar #'ebdb-string
					(ebdb-available-ebdb-formatters)))
	       (mapcar
		(lambda (s) (cons (ebdb-string s) s))
		(ebdb-available-ebdb-formatters))))))
  (ebdb-change-records-fmt (ebdb-record-list records t) fmt))

;;;###autoload
(defun ebdb-omit-record (n)
  "Remove current record from the display without deleting it from EBDB.
With prefix N, omit the next N records.  If negative, omit backwards."
  (interactive "p")
  (let ((num  (get-text-property (if (and (not (bobp)) (eobp))
                                     (1- (point)) (point))
                                 'ebdb-record-number)))
    (if (> n 0)
        (setq n (min n (- (length ebdb-records) num)))
      (setq n (min (- n) num))
      (ebdb-prev-record n))
    (dotimes (_ n)
      (ebdb-redisplay-record (ebdb-current-record) nil t))))

;; Entry points to EBDB

;;;###autoload
(defun ebdb (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP
in either the name(s), organization, address, phone, mail, or xfields."
  (interactive (list (ebdb-search-read) (ebdb-formatter-prefix)))
  (let ((records (ebdb-search (ebdb-records) regexp regexp regexp
                              regexp (cons '* regexp) regexp regexp)))
    (if records
        (ebdb-display-records records fmt nil t)
      (message "No records matching '%s'" regexp))))

;;;###autoload
(defun ebdb-search-name (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP in the name
\(or ``alternate'' names\)."
  (interactive (list (ebdb-search-read "names") (ebdb-formatter-prefix)))
  (ebdb-display-records (ebdb-search (ebdb-records) regexp) fmt))

;;;###autoload
(defun ebdb-search-organization (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP in the organization field."
  (interactive (list (ebdb-search-read "organization") (ebdb-formatter-prefix)))
  (ebdb-display-records
   (ebdb-search (ebdb-records 'ebdb-record-organization t)
		regexp)
   fmt))

;;;###autoload
(defun ebdb-search-address (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP in the address fields."
  (interactive (list (ebdb-search-read "address") (ebdb-formatter-prefix)))
  (ebdb-display-records
   (seq-filter
    (lambda (r)
      (ebdb-record-search r 'address regexp))
    (ebdb-records))
   fmt))

;;;###autoload
(defun ebdb-search-mail (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP in the mail address."
  (interactive (list (ebdb-search-read "mail address") (ebdb-formatter-prefix)))
  (ebdb-display-records
   (seq-filter
    (lambda (r)
      (ebdb-record-search r 'mail regexp))
    (ebdb-records))
   fmt))

;;;###autoload
(defun ebdb-search-phone (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP in the phones field."
  (interactive (list (ebdb-search-read "phone") (ebdb-formatter-prefix)))
  (ebdb-display-records
   (seq-filter
    (lambda (r)
      (ebdb-record-search r 'phone regexp))
    (ebdb-records))
   fmt))

;;;###autoload
(defun ebdb-search-notes (regexp &optional fmt)
  "Display all records in the EBDB matching REGEXP in the phones field."
  (interactive (list (ebdb-search-read "notes") (ebdb-formatter-prefix)))
  (ebdb-display-records
   (seq-filter
    (lambda (r)
      (ebdb-record-search r 'notes regexp))
    (ebdb-records))
   fmt))

;;;###autoload
(defun ebdb-search-user-fields (field regexp &optional fmt)
  "Display all EBDB records for which user field FIELD matches REGEXP."
  (interactive
   ;; TODO: Refactor this with `ebdb-prompt-for-field-type'
   (let* ((field-alist
	  (append
	   (mapcar
	    (lambda (f)
	      (cons
	       (ebdb-field-readable-name (intern (car f)))
	       (intern (car f))))
	    (eieio-build-class-alist 'ebdb-field-user t))
	   (mapcar
	    (lambda (l)
	      (cons l 'ebdb-field-user-simple))
	    ebdb-user-label-list)))
	 (field (assoc (completing-read "Field to search (RET for all): "
					field-alist
					nil t)
		       field-alist)))
     (list (if (null field) '* field)
           (ebdb-search-read (if (null field)
				 "any user field"
			       (car field)))
           (ebdb-formatter-prefix))))
  (ebdb-display-records (ebdb-search (ebdb-records) nil nil nil nil
                                     (cons field regexp))
                        fmt))

;;;###autoload
(defun ebdb-search-changed (&optional fmt)
  ;; FIXME: "changes" in EBDB lingo are often called "modifications"
  ;; in Emacs lingo
  "Display records which have been changed since EBDB was last saved."
  (interactive (list (ebdb-formatter-prefix)))
  (let ((dirty (ebdb-dirty-records)))
    (if (ebdb-search-invert-p)
	(let (unchanged-records)
	  (dolist (record (ebdb-records))
	    (unless (memq record dirty)
	      (push record unchanged-records)))
	  (ebdb-display-records unchanged-records fmt))
      (ebdb-display-records dirty fmt))))

(defun ebdb-search-prog (function &optional fmt)
  "Search records using FUNCTION.
FUNCTION is called with one argument, the record, and should return
the record to be displayed or nil otherwise."
  (ebdb-display-records (seq-filter function (ebdb-records)) fmt))

;;; Send-Mail interface

;;;###autoload
(defun ebdb-dwim-mail (record &optional mail)
  ;; Do What I Mean!
  "Return a string to use as the mail address of RECORD.
The name in the mail address is formatted obeying `ebdb-mail-name-format'
and `ebdb-mail-name'.  However, if both the first name and last name
are constituents of the address as in John.Doe@Some.Host,
and `ebdb-mail-avoid-redundancy' is non-nil, then the address is used as is
and `ebdb-mail-name-format' and `ebdb-mail-name' are ignored.
If `ebdb-mail-avoid-redundancy' is 'mail-only the name is never included.
MAIL may be a mail address to be used for RECORD.
If MAIL is an integer, use the MAILth mail address of RECORD.
If MAIL is nil use RECORD's primary mail address."
  (unless mail
    (let ((mails (ebdb-record-mail record t)))
      (setq mail (or (and (integerp mail) (nth mail mails))
                     (object-assoc 'primary 'priority mails)
		     (car mails)))))
  (unless mail (error "Record has no mail addresses"))
  (let ((mail-aka (slot-value mail 'aka))
	name fn ln)
    (setq mail (slot-value mail 'mail))
    (cond (mail-aka
	   (setq name mail-aka))
          ((functionp ebdb-mail-name)
           (setq name (funcall ebdb-mail-name record))
           (if (consp name)
               (setq fn (car name) ln (cdr name)
                     name (if (eq ebdb-mail-name-format 'first-last)
                              (ebdb-concat 'name-first-last fn ln)
                            (ebdb-concat 'name-last-first ln fn)))
             (let ((pair (ebdb-divide-name name)))
               (setq fn (car pair) ln (cdr pair)))))
          ((setq name (ebdb-record-user-field record ebdb-mail-name))
           (let ((pair (ebdb-divide-name name)))
             (setq fn (car pair) ln (cdr pair))))
          (t
           (setq name (if (eq ebdb-mail-name-format 'first-last)
                          (ebdb-record-name record)
                        (ebdb-name-lf (slot-value record 'name)))
                 fn (ebdb-record-firstname record)
                 ln (ebdb-record-lastname  record))))
    (if (or (not name) (equal "" name)
            (eq 'mail-only ebdb-mail-avoid-redundancy)
            (and ebdb-mail-avoid-redundancy
                 (cond ((and fn ln)
                        (let ((fnq (regexp-quote fn))
                              (lnq (regexp-quote ln)))
                          (or (string-match (concat "\\`[^!@%]*\\b" fnq
                                                    "\\b[^!%@]+\\b" lnq "\\b")
                                            mail)
                            (string-match (concat "\\`[^!@%]*\\b" lnq
                                                  "\\b[^!%@]+\\b" fnq "\\b")
                                          mail))))
                       ((or fn ln)
                        (string-match (concat "\\`[^!@%]*\\b"
                                              (regexp-quote (or fn ln)) "\\b")
                                      mail)))))
        mail
      ;; If the name contains backslashes or double-quotes, backslash them.
      (setq name (replace-regexp-in-string "[\\\"]" "\\\\\\&" name))
      ;; If the name contains control chars or RFC822 specials, it needs
      ;; to be enclosed in quotes.  This quotes a few extra characters as
      ;; well (!,%, and $) just for common sense.
      ;; `define-mail-alias' uses regexp "[^- !#$%&'*+/0-9=?A-Za-z^_`{|}~]".
      (format (if (string-match "[][[:cntrl:]\177()<>@,;:.!$%[:nonascii:]]" name)
                  "\"%s\" <%s>"
                "%s <%s>")
              name mail))))

(defun ebdb-compose-mail (&rest args)
  "Start composing a mail message to send."
  (apply 'compose-mail args))

;;;###autoload
(defun ebdb-mail (records &optional subject n verbose)
  "Compose a mail message to RECORDS (optional: using SUBJECT).
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (ebdb-do-records) nil
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t))
  (setq records (ebdb-record-list records))
  (if (not records)
      (if verbose (message "No records"))
    (let ((to (ebdb-mail-address records n nil verbose)))
      (unless (string= "" to)
        (ebdb-compose-mail to subject)))))

(defun ebdb-mail-address (records &optional n kill-ring-save verbose)
  "Return mail addresses of RECORDS as a string.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
By default, the first mail addresses of RECORDS are used.
If prefix N is a number, use Nth mail address of RECORDS (starting from 1).
If prefix N is C-u (t noninteractively) use all mail addresses of RECORDS.
If KILL-RING-SAVE is non-nil (as in interactive calls), copy mail addresses
to kill ring.  If VERBOSE is non-nil (as in interactive calls) be verbose."
  (interactive (list (ebdb-do-records)
                     (or (consp current-prefix-arg)
                         current-prefix-arg)
                     t t))
  (setq records (ebdb-record-list records))
  (if (not records)
      (progn (if verbose (message "No records")) "")
    (let ((good "") bad)
      (dolist (record records)
        (let ((mails (ebdb-record-mail record t)))
          (cond ((not mails)
                 (push record bad))
                ((eq n t)
                 (setq good (ebdb-concat ",\n\t"
                                         good
                                         (mapcar (lambda (mail)
                                                   (ebdb-dwim-mail record mail))
                                                 mails))))
                (t
                 (setq good (ebdb-concat ",\n\t" good
                            (ebdb-dwim-mail record (and (numberp n)
							(nth (1- n) mails)))))))))
      (when (and bad verbose)
        (message "No mail addresses for %s."
                 (mapconcat 'ebdb-string (nreverse bad) ", "))
        (unless (string= "" good) (sit-for 2)))
      (when (and kill-ring-save (not (string= good "")))
        (kill-new good)
        (if verbose (message "%s" good)))
      good)))

;; Is there better way to yank selected mail addresses from the EBDB
;; buffer into a message buffer?  We need some kind of a link between
;; the EBDB buffer and the message buffer, where the mail addresses
;; are supposed to go. Then we could browse the EBDB buffer and copy
;; selected mail addresses from the EBDB buffer into a message buffer.

(defun ebdb-mail-yank ()
  "CC the people displayed in the *EBDB* buffer on this mail message.
The primary mail of each of the records currently listed in the
*EBDB* buffer will be appended to the CC: field of the current buffer."
  (interactive)
  (let ((addresses (with-current-buffer ebdb-buffer-name
                     (delq nil
                           (mapcar (lambda (x)
                                     (if (ebdb-record-mail (car x))
                                         (ebdb-dwim-mail (car x))))
                                   ebdb-records))))
        (case-fold-search t))
    (goto-char (point-min))
    (if (re-search-forward "^CC:[ \t]*" nil t)
        ;; We have a CC field. Move to the end of it, inserting a comma
        ;; if there are already addresses present.
        (unless (eolp)
          (end-of-line)
          (while (looking-at "\n[ \t]")
            (forward-char) (end-of-line))
          (insert ",\n")
          (indent-relative))
      ;; Otherwise, if there is an empty To: field, move to the end of it.
      (unless (and (re-search-forward "^To:[ \t]*" nil t)
                   (eolp))
        ;; Otherwise, insert an empty CC: field.
        (end-of-line)
        (while (looking-at "\n[ \t]")
          (forward-char) (end-of-line))
        (insert "\nCC:")
        (indent-relative)))
    ;; Now insert each of the addresses on its own line.
    (while addresses
      (insert (car addresses))
      (when (cdr addresses) (insert ",\n") (indent-relative))
      (setq addresses (cdr addresses)))))
(define-obsolete-function-alias 'ebdb-yank-addresses 'ebdb-mail-yank)

;;; completion

;;;###autoload
(defun ebdb-completion-predicate (key records)
  "For use as the third argument to `completing-read'.
Obey `ebdb-completion-list'."
  (cond ((null ebdb-completion-list)
         nil)
        ((eq t ebdb-completion-list)
         t)
        (t
         (catch 'ebdb-hash-ok
	   (dolist (record records)
	     (ebdb-hash-p key record ebdb-completion-list))
	   nil))))

(defun ebdb-completing-read-records (prompt &optional omit-records)
  "Read and return list of records from the ebdb.
Completion is done according to `ebdb-completion-list'.  If the user
just hits return, nil is returned.  Otherwise, a valid response is forced."
  (let* ((completion-ignore-case t)
         (string (completing-read prompt ebdb-hashtable
                                  'ebdb-completion-predicate t)))
    (unless (string= "" string)
      (let (records)
	(dolist (record (gethash string ebdb-hashtable))
	  (when (and (not (memq record omit-records))
		     (not (memq record records)))
	    (push record records)))
	records))))
	 

(defun ebdb-completing-read-record (prompt &optional omit-records)
  "Prompt for and return a single record from the ebdb;
completion is done according to `ebdb-completion-list'.  If the user
just hits return, nil is returned. Otherwise, a valid response is forced.
If OMIT-RECORDS is non-nil it should be a list of records to dis-allow
completion with."
  (let ((records (ebdb-completing-read-records prompt omit-records)))
    (cond ((eq (length records) 1)
           (car records))
          ((> (length records) 1)
           (ebdb-display-records records ebdb-default-oneline-formatter)
           (let* ((count (length records))
                  (result (completing-read
                           (format "Which record (1-%s): " count)
                           (mapcar 'number-to-string (number-sequence 1 count))
                           nil t)))
             (nth (1- (string-to-number result)) records))))))

;;;###autoload
(defun ebdb-completing-read-mails (prompt &optional init)
  "Like `read-string', but allows `ebdb-complete-mail' style completion."
  (read-from-minibuffer prompt init
                        ebdb-completing-read-mails-map))

(defconst ebdb-quoted-string-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "\"" st)
    st)
  "Syntax-table to parse matched quotes.  Used by `ebdb-complete-mail'.")

;;;###autoload
(defun ebdb-complete-mail (&optional beg cycle-completion-buffer)
  "In a mail buffer, complete the user name or mail before point.
Completion happens up to the preceeding colon, comma, or BEG.
Return non-nil if there is a valid completion, else return nil.

Completion behaviour obeys `ebdb-completion-list' (see there).
If what has been typed matches a unique EBDB record, insert an address
formatted by `ebdb-dwim-mail' (see there).  Also, display this record
if `ebdb-completion-display-record' is non-nil,
If what has been typed is a valid completion but does not match
a unique record, display a list of completions.
If the completion is done and `ebdb-complete-mail-allow-cycling' is t
then cycle through the mails for the matching record.  If EBDB
would format a given address different from what we have in the mail buffer,
the first round of cycling reformats the address accordingly, then we cycle
through the mails for the matching record.
With prefix CYCLE-COMPLETION-BUFFER non-nil, display a list of all mails
available for cycling.

Set the variable `ebdb-complete-mail' non-nil for enabling this feature
as part of the MUA insinuation."
  (interactive (list nil current-prefix-arg))

  (unless ebdb-db-list
    (ebdb-load))

  ;; Completion should begin after the preceding comma (separating
  ;; two addresses) or colon (separating the header field name
  ;; from the header field body).  We want to ignore these characters
  ;; if they appear inside a quoted string (RFC 5322, Sec. 3.2.4).
  ;; Note also that a quoted string may span multiple lines
  ;; (RFC 5322, Sec. 2.2.3).
  ;; So to be save, we go back to the beginning of the header field body
  ;; (past the colon, when we are certainly not inside a quoted string),
  ;; then we parse forward, looking for commas not inside a quoted string
  ;; and positioned before END.  - This fails with an unbalanced quote.
  ;; But an unbalanced quote is bound to fail anyway.
  (when (and (not beg)
             (<= (point)
                 (save-restriction	; `mail-header-end'
                   (widen)
                   (save-excursion
                     (rfc822-goto-eoh)
                     (point)))))
    (let ((end (point))
          start pnt state)
      (save-excursion
        ;; A header field name must appear at the beginning of a line,
        ;; and it must be terminated by a colon.
        (re-search-backward "^[^ \t\n:][^:]*:[ \t\n]+")
        (setq beg (match-end 0)
              start beg)
        (goto-char beg)
        ;; If we are inside a syntactically correct header field,
        ;; all continuation lines in between the field name and point
        ;; must begin with a white space character.
        (if (re-search-forward "\n[^ \t]" end t)
            ;; An invalid header is identified via BEG set to nil.
            (setq beg nil)
          ;; Parse field body up to END
          (with-syntax-table ebdb-quoted-string-syntax-table
            (while (setq pnt (re-search-forward ",[ \t\n]*" end t))
              (setq state (parse-partial-sexp start pnt nil nil state)
                    start pnt)
              (unless (nth 3 state) (setq beg pnt))))))))

  ;; Do we have a meaningful way to set BEG if we are not in a message header?
  (unless beg
    (message "Not a valid buffer position for mail completion")
    (sit-for 1))

  (let* ((end (point))
         (done (unless beg 'nothing))
         (orig (and beg (buffer-substring-no-properties beg end)))
         (completion-ignore-case t)
         (completion (and orig
                          (try-completion orig ebdb-hashtable
                                          'ebdb-completion-predicate)))
         all-completions dwim-completions one-record)

    (unless done
      ;; We get fooled if a partial COMPLETION matches "," (for example,
      ;; a comma in lf-name).  Such a partial COMPLETION cannot be protected
      ;; by quoting.  Then the comma gets interpreted as BEG.
      ;; So we never perform partial completion beyond the first comma.
      ;; This works even if we have just one record matching ORIG (thus
      ;; allowing dwim-completion) because ORIG is a substring of COMPLETION
      ;; even after COMPLETION got truncated; and ORIG by itself must be
      ;; sufficient to identify this record.
      ;; Yet if multiple records match ORIG we can only offer a *Completions*
      ;; buffer.
      (if (and (stringp completion)
               (string-match "," completion))
          (setq completion (substring completion 0 (match-beginning 0))))

      (setq all-completions (all-completions orig ebdb-hashtable
                                             'ebdb-completion-predicate))

      ;; Resolve the records matching ORIG:
      ;; Multiple completions may match the same record
      (let ((records (delete-dups
                      (apply 'append (mapcar (lambda (compl)
					       (gethash compl ebdb-hashtable))
					     all-completions)))))
        ;; Is there only one matching record?
        (setq one-record (and (not (cdr records))
                              (car records))))

      ;; Clean up *Completions* buffer window, if it exists
      (let ((window (get-buffer-window "*Completions*")))
        (if (window-live-p window)
            (quit-window nil window)))

      (cond
       ;; Match for a single record
       (one-record
        (let ((completion-list (if (eq t ebdb-completion-list)
                                   '(name alt-names mail aka organization)
                                 ebdb-completion-list))
              (mails (ebdb-record-mail one-record t))
              mail elt)
          (if (not mails)
              (progn
                (message "Matching record has no mail field")
                (sit-for 1)
                (setq done 'nothing))

            ;; Determine the mail address of ONE-RECORD to use for ADDRESS.
            ;; Do we have a preferential order for the following tests?
            ;; (1) If ORIG matches name, AKA, or organization of ONE-RECORD,
            ;;     then ADDRESS will be the first mail address of ONE-RECORD.
            (if (try-completion orig
                                (append
                                 (if (memq 'name completion-list)
                                     (list (or (ebdb-record-name one-record) "")))
                                 (if (memq 'alt-names completion-list)
				     (or (ebdb-record-alt-names one-record) (list "")))
                                 (if (memq 'organization completion-list)
                                     (ebdb-record-organizations one-record))))
                (setq mail (car mails)))
            ;; (2) If ORIG matches one or multiple mail addresses of ONE-RECORD,
            ;;     then we take the first one matching ORIG.
            ;;     We got here with MAIL nil only if `ebdb-completion-list'
            ;;     includes 'mail or 'primary.
            (unless mail
              (while (setq elt (pop mails))
                (if (try-completion orig (list (ebdb-string elt)))
                    (setq mail elt
                          mails nil))))
            ;; This error message indicates a bug!
            (unless mail (error "No match for %s" orig))

            (let ((dwim-mail (ebdb-dwim-mail one-record mail)))
              (if (string= dwim-mail orig)
                  ;; We get here if `ebdb-mail-avoid-redundancy' is 'mail-only
                  ;; and `ebdb-completion-list' includes 'mail.
                  (unless (and ebdb-complete-mail-allow-cycling
                               (< 1 (length (ebdb-record-mail one-record))))
                    (setq done 'unchanged))
                ;; Replace the text with the expansion
                (delete-region beg end)
                (insert dwim-mail)
                (ebdb-complete-mail-cleanup dwim-mail beg)
                (setq done 'unique))))))

       ;; Partial completion
       ((and (stringp completion)
             (not (ebdb-string= orig completion)))
        (delete-region beg end)
        (insert completion)
        (setq done 'partial))

       ;; Partial match not allowing further partial completion
       (completion
        (let ((completion-list (if (eq t ebdb-completion-list)
                                   '(name alt-names mail organization)
                                 ebdb-completion-list)))
          ;; Now collect all the dwim-addresses for each completion.
          ;; Add it if the mail is part of the completions
          (dolist (key all-completions)
            (dolist (record (gethash key ebdb-hashtable))
              (let ((mails (ebdb-record-mail record t))
                    accept)
                (when mails
                  (dolist (field completion-list)
                    (cond ((eq field 'name)
                           (if (ebdb-string= key (ebdb-record-name record))
                               (push (car mails) accept)))
                          ((eq field 'alt-names)
                           (if (member-ignore-case
				key (ebdb-record-alt-names record))
                               (push (car mails) accept)))
                          ((eq field 'organization)
                           (if (member-ignore-case key (ebdb-record-organizations
							record))
                               (push (car mails) accept)))
                          ((eq field 'primary)
                           (if (ebdb-string= key (ebdb-string
						  (car mails)))
                               (push (car mails) accept)))
                          ((eq field 'mail)
                           (dolist (mail mails)
                             (if (ebdb-string= key
					       (ebdb-string mail))
                                 (push mail accept))))))
                  (dolist (mail (delete-dups accept))
                    (push (ebdb-dwim-mail record mail) dwim-completions))))))

          (setq dwim-completions (sort (delete-dups dwim-completions)
                                       'string-lessp))
          (cond ((not dwim-completions)
                 (message "Matching record has no mail field")
                 (sit-for 1)
                 (setq done 'nothing))
                ;; DWIM-COMPLETIONS may contain only one element,
                ;; if multiple completions match the same record.
                ;; Then we may proceed with DONE set to `unique'.
                ((eq 1 (length dwim-completions))
                 (delete-region beg end)
                 (insert (car dwim-completions))
                 (ebdb-complete-mail-cleanup (car dwim-completions) beg)
                 (setq done 'unique))
                (t (setq done 'choose)))))))

    ;; By now, we have considered all possiblities to perform a completion.
    ;; If nonetheless we haven't done anything so far, consider cycling.
    ;;
    ;; Completion and cycling are really two very separate things.
    ;; Completion is controlled by the user variable `ebdb-completion-list'.
    ;; Cycling assumes that ORIG already holds a valid RFC 822 mail address.
    ;; Therefore cycling may consider different records than completion.
    (when (and (not done) ebdb-complete-mail-allow-cycling)
      ;; find the record we are working on.
      (let* ((address (ebdb-extract-address-components orig))
             (record (car (ebdb-message-search
                           (car address) (cadr address)))))
        (if (and record
                 (setq dwim-completions
                       (mapcar (lambda (m) (ebdb-dwim-mail record m))
                               (ebdb-record-mail record t))))
            (cond ((and (= 1 (length dwim-completions))
                        (string= orig (car dwim-completions)))
                   (setq done 'unchanged))
                  (cycle-completion-buffer ; use completion buffer
                   (setq done 'cycle-choose))
                  ;; Reformatting / Clean up:
                  ;; If the canonical mail address (nth 1 address)
                  ;; matches the Nth canonical mail address of RECORD,
                  ;; but ORIG is not `equal' to (ebdb-dwim-mail record n),
                  ;; then we replace ORIG by (ebdb-dwim-mail record n).
                  ;; For example, the address "JOHN SMITH <FOO@BAR.COM>"
                  ;; gets reformatted as "John Smith <foo@bar.com>".
                  ;; We attempt this reformatting before the yet more
                  ;; aggressive proper cycling.
		  ((let* ((canon (car (member-ignore-case
				       (nth 1 address)
				       (ebdb-record-mail-canon record))))
			  (dwim-mail (let ((case-fold-search t))
				       (seq-find (lambda (d)
						   (string-match-p canon d))
						 dwim-completions))))
		     (when (not (string= orig dwim-mail))
		       (delete-region beg end)
		       (insert dwim-mail)
		       (ebdb-complete-mail-cleanup dwim-mail beg)
		       (setq done 'reformat))
		     done))
                  (t
                   ;; ORIG is `equal' to an element of DWIM-COMPLETIONS
                   ;; Use the next element of DWIM-COMPLETIONS.
                   (let ((dwim-mail (or (nth 1 (member orig dwim-completions))
                                        (nth 0 dwim-completions))))
                     ;; replace with new mail address
                     (delete-region beg end)
                     (insert dwim-mail)
                     (ebdb-complete-mail-cleanup dwim-mail beg)
                     (setq done 'cycle)))))))

    (when (member done '(choose cycle-choose))
      ;; Pop up a completions window using DWIM-COMPLETIONS.
      ;; Too bad: The following requires at least GNU Emacs 23.2
      ;; which introduced the variable `completion-base-position'.
      ;; For an older Emacs there is really no satisfactory workaround
      ;; (see GNU Emacs bug #4699), unless we use something radical like
      ;; advicing `choose-completion-string' (used by EBDB v2).
      (if (string< (substring emacs-version 0 4) "23.2")
          (message "*Completions* buffer requires at least GNU Emacs 23.2")
        ;; `completion-in-region' does not work here as `dwim-completions'
        ;; is not a collection for completion in the usual sense, but it
        ;; is really a list of replacements.
        (let ((status (not (eq (selected-window) (minibuffer-window))))
              (completion-base-position (list beg end))
              ;; If we even have `completion-list-insert-choice-function'
              ;; (introduced in GNU Emacs 24.1) that is yet better.
              ;; Then we first call the default value of this variable
              ;; before performing our own stuff.
              (completion-list-insert-choice-function
               `(lambda (beg end text)
                  ,(if (boundp 'completion-list-insert-choice-function)
		       `(funcall ',completion-list-insert-choice-function
				 beg end text))
		  (ebdb-complete-mail-cleanup text beg))))
          (if status (message "Making completion list..."))
          (with-output-to-temp-buffer "*Completions*"
            (display-completion-list dwim-completions))
          (if status (message "Making completion list...done")))))

    ;; If DONE is `nothing' return nil so that possibly some other code
    ;; can take over.
    (unless (eq done 'nothing)
      done)))

(defun ebdb-complete-mail-cleanup (mail beg)
  "Clean up after inserting MAIL at position BEG.
If we are past `fill-column', wrap at the previous comma."
  (if (and (not (auto-fill-function))
           (>= (current-column) fill-column))
      (save-excursion
        (goto-char beg)
        (when (search-backward "," (line-beginning-position) t)
          (forward-char 1)
          (insert "\n")
          (indent-relative)
          (if (looking-at "[ \t\n]+")
              (delete-region (point) (match-end 0))))))
  (if (or ebdb-completion-display-record ebdb-complete-mail-hook)
      (let* ((address (ebdb-extract-address-components mail))
             (records (ebdb-message-search (car address) (nth 1 address))))
        ;; Update the *EBDB* buffer if desired.
        (if ebdb-completion-display-record
            (let ((ebdb-silent-internal t))
              ;; FIXME: This pops up *EBDB* before removing *Completions*
              (ebdb-display-records records nil t)))
        ;; `ebdb-complete-mail-hook' may access MAIL, ADDRESS, and RECORDS.
        (run-hooks 'ebdb-complete-mail-hook))))

;;; interface to mail-abbrevs.el.

;; Just stub this out for now.
;;;###autoload
(defun ebdb-mail-aliases (&optional _force-rebuilt _noisy)
  t)
;; (defun ebdb-mail-aliases (&optional force-rebuilt noisy)
;;   "Define mail aliases for the records in the database.
;; Define a mail alias for every record that has a `mail-alias' field
;; which is the contents of that field.
;; If there are multiple comma-separated words in the `mail-alias' field,
;; then all of those words will be defined as aliases for that person.

;; If multiple records in the database have the same mail alias,
;; then that alias expands to a comma-separated list of the mail addresses
;; of all of these people.
;; Add this command to `mail-setup-hook'.

;; Mail aliases are (re)built only if `ebdb-mail-aliases-need-rebuilt' is non-nil
;; because the database was newly loaded or it has been edited.
;; Rebuilding the aliases is enforced if prefix FORCE-REBUILT is t."
;;   (interactive (list current-prefix-arg t))
;;   ;; Build `mail-aliases' if not yet done.
;;   ;; Note: `mail-abbrevs-setup' rebuilds the mail-aliases only if
;;   ;; `mail-personal-alias-file' has changed.  So it would not do anything
;;   ;; if we want to rebuild the mail-aliases because of changes in EBDB.
;;   (if (or force-rebuilt (eq t mail-aliases)) (build-mail-aliases))

;;   ;; We should be cleverer here and instead of rebuilding all aliases
;;   ;; we should just do what's necessary, i.e. remove deleted records
;;   ;; and add new records
;;   ;; Calling `ebdb-records' can change `ebdb-mail-aliases-need-rebuilt'
;;   (let ((records (ebdb-search (ebdb-records) nil nil nil
;;                               (cons ebdb-mail-alias-field ".")))
;;         results match)
;;     (if (not (or force-rebuilt ebdb-mail-aliases-need-rebuilt))
;;         (if noisy (message "EBDB mail alias: nothing to do"))
;;       (setq ebdb-mail-aliases-need-rebuilt nil)

;;       ;; collect an alist of (alias rec1 [rec2 ...])
;;       (dolist (record records)
;;         (if (ebdb-record-mail record)
;;             (dolist (alias (ebdb-record-xfield-split record ebdb-mail-alias-field))
;;               (if (setq match (assoc alias results))
;;                   ;; If an alias appears more than once, we collect all records
;;                   ;; that refer to it.
;;                   (nconc match (list record))
;;                 (push (list alias record) results)))
;;           (unless ebdb-silent
;;             (ebdb-warn "record %S has no mail address, but the aliases: %s"
;;                        (ebdb-record-name record)
;;                        (ebdb-record-xfield record ebdb-mail-alias-field))
;;             (sit-for 1))))

;;       ;; Iterate over the results and create the aliases
;;       (dolist (result results)
;;         (let* ((aliasstem (car result))
;;                (expansions
;;                 (if (cddr result)
;;                     ;; for group aliases we just take all the primary mails
;;                     ;; and define only one expansion!
;;                     (list (mapconcat (lambda (record) (ebdb-dwim-mail record))
;;                                      (cdr result) mail-alias-separator-string))
;;                   ;; this is an alias for a single person so deal with it
;;                   ;; according to `ebdb-mail-alias'
;;                   (let* ((record (nth 1 result))
;;                          (mails (ebdb-record-mail record)))
;;                     (if (or (eq 'first ebdb-mail-alias)
;;                             (not (cdr mails)))
;;                         ;; Either we want to define only one alias for
;;                         ;; the first mail address or there is anyway
;;                         ;; only one address.  In either case, we take
;;                         ;; take only the first address.
;;                         (list (ebdb-dwim-mail record (car mails)))
;;                       ;; We need to deal with more than one mail address...
;;                       (let* ((all (mapcar (lambda (m) (ebdb-dwim-mail record m))
;;                                           mails))
;;                              (star (ebdb-concat mail-alias-separator-string all)))
;;                         (if (eq 'star ebdb-mail-alias)
;;                             (list star (car all))
;;                           ;; if `ebdb-mail-alias' is 'all, we create
;;                           ;; two aliases for the primary mail address
;;                           (cons star (cons (car all) all))))))))
;;                (count -1) ; n=-1: <alias>*;  n=0: <alias>;  n>0: <alias>n
;;                (len (length expansions))
;;                alias f-alias)

;;           ;; create the aliases for each expansion
;;           (dolist (expansion expansions)
;;             (cond ((or (= 1 len)
;;                        (= count 0))
;;                    (setq alias aliasstem))
;;                   ((= count -1) ;; all the mails of a record
;;                    (setq alias (concat aliasstem "*")))
;;                   (t ;; <alias>n for each mail of a record
;;                    (setq alias (format "%s%s" aliasstem count))))
;;             (setq count (1+ count))

;;             (add-to-list 'mail-aliases (cons alias expansion))

;;             (define-mail-abbrev alias expansion)
;;             (unless (setq f-alias (intern-soft (downcase alias) mail-abbrevs))
;;               (error "Cannot find the alias"))

;;             ;; `define-mail-abbrev' initializes f-alias to be
;;             ;; `mail-abbrev-expand-hook'. We replace this by
;;             ;; `ebdb-mail-abbrev-expand-hook'
;;             (unless (eq (symbol-function f-alias) 'mail-abbrev-expand-hook)
;;               (error "mail-aliases contains unexpected hook %s"
;;                      (symbol-function f-alias)))
;;             ;; `ebdb-mail-abbrev-hook' is called with mail addresses instead of
;;             ;; ebdb records to avoid keeping pointers to records, which would
;;             ;; lose if the database was reverted.
;;             ;; `ebdb-mail-abbrev-hook' uses `ebdb-message-search' to convert
;;             ;; these mail addresses to records, which is plenty fast.
;;             ;; FIXME: The value of arg MAILS for `ebdb-mail-abbrev-hook'
;;             ;; is wrong. Currently it is based on the list of records that have
;;             ;; referenced ALIASTEM and we simply take the first mail address
;;             ;; from each of these records.
;;             ;; Then `ebdb-message-search' will find the correct records
;;             ;; (assuming that each mail address appears only once in the
;;             ;; database).  Nonethless, arg MAILS for `ebdb-mail-abbrev-hook'
;;             ;; does not, in general, contain the actual mail addresses
;;             ;; of EXPANSION.  So what we would need is to go back from
;;             ;; EXPANSION to the mail addresses it contains (which is tricky
;;             ;; because mail addresses in the database can be shortcuts for
;;             ;; the addresses in EXPANSION).
;;             (fset f-alias `(lambda ()
;;                              (ebdb-mail-abbrev-expand-hook
;;                               ,alias
;;                               ',(mapcar (lambda (r) (car (ebdb-record-mail r)))
;;                                         (cdr result))))))))

;;       (if noisy (message "EBDB mail alias: rebuilding done")))))

;; (defun ebdb-mail-abbrev-expand-hook (alias mails)
;;   (run-hook-with-args 'ebdb-mail-abbrev-expand-hook alias mails)
;;   (mail-abbrev-expand-hook)
;;   (when ebdb-completion-display-record
;;     (let ((ebdb-silent-internal t))
;;       (ebdb-display-records
;;        (apply 'append
;;               (mapcar (lambda (mail) (ebdb-message-search nil mail)) mails))
;;        nil t))))

;; (defun ebdb-get-mail-aliases ()
;;   "Return a list of mail aliases used in the EBDB."
;;   (let ((records (ebdb-search (ebdb-records) nil nil nil
;;                               (cons ebdb-mail-alias-field ".")))
;;         result)
;;     (dolist (record records result)
;;       (dolist (alias (ebdb-record-xfield-split record ebdb-mail-alias-field))
;;         (add-to-list 'result alias)))))

;; ;;;###autoload
;; (defsubst ebdb-mail-alias-list (alias)
;;   (if (stringp alias)
;;       (ebdb-split ebdb-mail-alias-field alias)
;;     alias))

;; (defun ebdb-add-mail-alias (records &optional alias delete)
;;   "Add ALIAS to RECORDS.
;; If prefix DELETE is non-nil, remove ALIAS from RECORDS.
;; Interactively, use EBDB prefix \
;; \\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
;; Arg ALIAS is ignored if list RECORDS contains more than one record.
;; Instead read ALIAS interactively for each record in RECORDS.
;; If the function `ebdb-init-mail-alias' is defined, it is called with
;; one arg RECORD to define the default value for ALIAS of RECORD."
;;   (interactive (list (ebdb-do-records) nil current-prefix-arg))
;;   (ebdb-editable)
;;   (setq records (ebdb-record-list records))
;;   (if (< 1 (length records)) (setq alias nil))
;;   (let* ((tmp (intern-soft
;;                (concat "ebdb-init-" (symbol-name ebdb-mail-alias-field))))
;;          (init-f (if (functionp tmp) tmp)))
;;     (dolist (record records)
;;       (let ((r-a-list (ebdb-record-xfield-split record ebdb-mail-alias-field))
;;             (alias alias)
;;             a-list)
;;         (if alias
;;             (setq a-list (ebdb-mail-alias-list alias))
;;           (when init-f
;;             (setq a-list (ebdb-mail-alias-list (funcall init-f record))
;;                   alias (if a-list (ebdb-concat ebdb-mail-alias-field a-list))))
;;           (let ((crm-separator
;;                  (concat "[ \t\n]*"
;;                          (cadr (assq ebdb-mail-alias-field ebdb-separator-alist))
;;                          "[ \t\n]*"))
;;                 (crm-local-completion-map ebdb-crm-local-completion-map)
;;                 (prompt (format "%s mail alias:%s " (if delete "Remove" "Add")
;;                                 (if alias (format " (default %s)" alias) "")))
;;                 (collection (if delete
;;                                 (or r-a-list (error "Record has no alias"))
;;                               (ebdb-get-mail-aliases))))
;;             (setq a-list (if (string< "24.3" (substring emacs-version 0 4))
;;                              (completing-read-multiple prompt collection nil
;;                                                        delete nil nil alias)
;;                           (ebdb-split ebdb-mail-alias-field
;;                                       (completing-read prompt collection nil
;;                                                        delete nil nil alias))))))
;;         (dolist (a a-list)
;;           (if delete
;;               (setq r-a-list (delete a r-a-list))
;;             ;; Add alias only if it is not there yet
;;             (add-to-list 'r-a-list a)))
;;         ;; This also handles `ebdb-mail-aliases-need-rebuilt'
;;         (ebdb-record-set-xfield record ebdb-mail-alias-field
;;                                 (ebdb-concat ebdb-mail-alias-field r-a-list))
;;         (ebdb-change-record record)))))

;;; Actions

(defun ebdb-record-action (arg record field)
  "Ask FIELD of RECORD to perform an action.

With ARG, use ARG an an index into FIELD's list of actions."
  (interactive (list current-prefix-arg
		     (ebdb-current-record)
		     (ebdb-current-field)))
  (ebdb-action field record arg))


;;; Dialing numbers from EBDB

(defun ebdb-dial-number (phone-string)
  "Dial the number specified by PHONE-STRING.
This uses the tel URI syntax passed to `browse-url' to make the call.
If `ebdb-dial-function' is non-nil then that is called to make the phone call."
  (interactive "sDial number: ")
  (if ebdb-dial-function
      (funcall ebdb-dial-function phone-string)
    (browse-url (concat "tel:" phone-string))))

;;;###autoload
(defun ebdb-dial (phone force-area-code)
  "Dial the number at point.
If the point is at the beginning of a record, dial the first phone number.
Use rules from `ebdb-dial-local-prefix-alist' unless prefix FORCE-AREA-CODE
is non-nil.  Do not dial the extension."
  (interactive (list (ebdb-current-field) current-prefix-arg))
  (if (eq phone 'ebdb-record-name)
      (setq phone (car (ebdb-record-phone (ebdb-current-record)))))
  (or (and (eieio-object-p phone)
	   (object-of-class-p phone 'ebdb-field-phone))
      (error "Not on a phone field"))

  (let ((number (ebdb-string phone))
        shortnumber)

    ;; cut off the extension
    (if (string-match "x[0-9]+$" number)
        (setq number (substring number 0 (match-beginning 0))))

    (unless force-area-code
      (let ((alist ebdb-dial-local-prefix-alist) prefix)
        (while (setq prefix (pop alist))
          (if (string-match (concat "^" (eval (car prefix))) number)
              (setq shortnumber (concat (cdr prefix)
                                        (substring number (match-end 0)))
                    alist nil)))))

    (if shortnumber
        (setq number shortnumber)

      ;; This is terrifically Americanized...
      ;; Leading 0 => local number (?)
      (if (and ebdb-dial-local-prefix
               (string-match "^0" number))
          (setq number (concat ebdb-dial-local-prefix number)))

      ;; Leading + => long distance/international number
      (if (and ebdb-dial-long-distance-prefix
               (string-match "^\+" number))
          (setq number (concat ebdb-dial-long-distance-prefix " "
                               (substring number 1)))))

    (unless ebdb-silent
      (message "Dialing %s" number))
    (ebdb-dial-number number)))

;;; url interface

;;;###autoload
(defun ebdb-browse-url (records &optional which)
  "Browse URLs stored in the `url' field of RECORDS.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'.
Prefix WHICH specifies which URL in field `url' is used (starting from 0).
Default is the first URL."
  (interactive (list (ebdb-get-records "Visit (URL): ")
                     (and current-prefix-arg
                          (prefix-numeric-value current-prefix-arg))))
  (unless which (setq which 0))
  (dolist (record (ebdb-record-list records))
    (let ((url (ebdb-record-xfield-split record 'url)))
      (when url
        (setq url (read-string "fetch: " (nth which url)))
        (unless (string= "" url)
          (browse-url url))))))

;;;###autoload
(defun ebdb-grab-url (record url label)
  "Grab URL and store it in RECORD."
  (interactive (let ((url (browse-url-url-at-point)))
                 (unless url (error "No URL at point"))
                 (list (ebdb-completing-read-record
                        (format "Add `%s' for: " url))
                       url
		       (ebdb-read-string "URL label: "
					 nil ebdb-url-label-list))))
  (let ((url-field (make-instance 'ebdb-field-url :url url :object-name label)))
      (ebdb-record-insert-field record 'fields url-field)
   (ebdb-display-records (list record))))

;;; Copy to kill ring

;;;###autoload
(defun ebdb-copy-records-as-kill (records)
  "Copy RECORDS to kill ring.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'."
  (interactive (list (ebdb-do-records t)))
  (let (drec)
    (dolist (record (ebdb-record-list records t))
      (push (buffer-substring (nth 2 record)
                              (or (nth 2 (car (cdr (memq record ebdb-records))))
                                  (point-max)))
            drec))
    (kill-new (replace-regexp-in-string
               "[ \t\n]*\\'" "\n"
               (mapconcat 'identity (nreverse drec) "")))))

;;;###autoload
(defun ebdb-copy-fields-as-kill (records field &optional num)
  "For RECORDS copy values of FIELD at point to kill ring.
If FIELD is an address or phone with a label, copy only field values
with the same label.  With numeric prefix NUM, if the value of FIELD
is a list, copy only the NUMth list element.
Interactively, use EBDB prefix \
\\<ebdb-mode-map>\\[ebdb-do-all-records], see `ebdb-do-all-records'."
  (interactive
   (list (ebdb-do-records t) (ebdb-current-field)
         (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))))
  (unless field (error "Not a field"))
  (let ((field-class (eieio-object-class field))
	val-list fields)
    ;; Store the first field string, then pop the record list.  If
    ;; there's only one record, this keeps things simpler.
    (push (ebdb-string field) val-list)
    (setq records (cdr (ebdb-record-list records)))
    (dolist (record records)
      (setq fields
	    (seq-filter
	     (lambda (f)
	       (same-class-p f field-class))
	     (mapcar #'cdr (ebdb-record-current-fields (car record)))))
      (when (object-of-class-p field 'ebdb-field-labeled)
	(setq fields
	      (seq-filter
	       (lambda (f)
		 (string= (eieio-object-name-string f)
			  (eieio-object-name-string field)))
	       fields)))
      (when (and num (> 1 (length fields)))
	(setq fields (list (nth num fields))))
      (dolist (f fields)
	(push (ebdb-string f) val-list)))
    (let ((str (ebdb-concat 'record (nreverse val-list))))
      (kill-new str)
      (message "%s" str))))



;;; Help and documentation

;;;###autoload
(defun ebdb-info ()
  (interactive)
  (info (format "(%s)Top" (or ebdb-info-file "ebdb"))))

;;;###autoload
(defun ebdb-help ()
  (interactive)
  (message (substitute-command-keys "\\<ebdb-mode-map>\
new field: \\[ebdb-insert-field]; \
edit field: \\[ebdb-edit-field]; \
delete field: \\[ebdb-delete-field-or-record]; \
mode help: \\[describe-mode]; \
info: \\[ebdb-info]")))

(provide 'ebdb-com)
;;; ebdb-com.el ends here
