;;; ebdb-gnus.el --- Gnus interface to EBDB          -*- lexical-binding: t; -*-

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

;; Code for interaction with Gnus.

;;; Code:

(require 'ebdb-com)
(require 'ebdb-mua)
(require 'gnus)

(autoload 'message-make-domain "message")

(defgroup ebdb-mua nil
  "Variables that specify the EBDB-MUA interface"
  :group 'ebdb)

(defgroup ebdb-mua-gnus nil
  "Gnus-specific EBDB customizations"
  :group 'ebdb-mua)
(put 'ebdb-mua-gnus 'custom-loads '(ebdb-gnus))

(defgroup ebdb-mua-gnus-scoring nil
  "Gnus-specific scoring EBDB customizations"
  :group 'ebdb-mua-gnus)
(put 'ebdb-mua-gnus-scoring 'custom-loads '(ebdb-gnus))

(defgroup ebdb-mua-gnus-splitting nil
  "Gnus-specific splitting EBDB customizations"
  :group 'ebdb-mua-gnus)
(put 'ebdb-mua-gnus-splitting 'custom-loads '(ebdb-gnus))

;;; Gnus-specific field types.  All should subclass
;;; `ebdb-field-user'.

(defclass ebdb-gnus-score-field (ebdb-field-user)
  ((score
    :type (or null number)
    :initarg :score
    :initval nil))
  :human-readable "gnus score")

(cl-defmethod ebdb-read ((field (subclass ebdb-gnus-score-field)) &optional slots obj)
  (let ((score (string-to-number
		(ebdb-read-string
		 "Score: " (when obj (slot-value obj 'score))))))
    (cl-call-next-method field (plist-put slots :score score) obj)))

(cl-defmethod ebdb-string ((field ebdb-gnus-score-field))
  (slot-value field 'score))

(defclass ebdb-gnus-private-field (ebdb-field-user)
  ((group
    :type string
    :initarg :group))
  :human-readable "gnus private")

(cl-defmethod ebdb-read ((field (subclass ebdb-gnus-private-field)) &optional slots obj)
  (let ((group (ebdb-read-string "Group name: " (when obj (slot-value obj 'group)))))
    (cl-call-next-method field (plist-put slots :group group) obj)))

(cl-defmethod ebdb-string ((field ebdb-gnus-private-field))
  (slot-value field 'group))

;; Scoring

(defcustom ebdb/gnus-score-default nil
  "If this is set, then every mail address in the EBDB that does not have
an associated score field will be assigned this score.  A value of nil
implies a default score of zero."
  :group 'ebdb-mua-gnus-scoring
  :type '(choice (const :tag "Do not assign default score" nil)
                 (integer :tag "Assign this default score" 0)))

(defvar ebdb/gnus-score-default-internal nil
  "Internal variable for detecting changes to
`ebdb/gnus-score-default'.  You should not set this variable directly -
set `ebdb/gnus-score-default' instead.")

(defvar ebdb/gnus-score-alist nil
  "The text version of the scoring structure returned by
ebdb/gnus-score.  This is built automatically from the EBDB.")

(defvar ebdb/gnus-score-rebuild-alist t
  "Set to t to rebuild ebdb/gnus-score-alist on the next call to
ebdb/gnus-score.  This will be set automatically if you change a EBDB
record which contains a gnus-score field.")

(defun ebdb/gnus-score-invalidate-alist (record)
  "This function is called through `ebdb-after-change-hook',
and sets `ebdb/gnus-score-rebuild-alist' to t if the changed
record contains a gnus-score field."
  (if (ebdb-record-user-field record 'ebdb-gnus-score-field)
      (setq ebdb/gnus-score-rebuild-alist t)))

;;;###autoload
(defun ebdb/gnus-score (group)
  "This returns a score alist for Gnus.  A score pair will be made for
every member of the mail field in records which also have a gnus-score
field.  This allows the EBDB to serve as a supplemental global score
file, with the advantage that it can keep up with multiple and changing
addresses better than the traditionally static global scorefile."
  (list (list
         (condition-case nil
             (read (ebdb/gnus-score-as-text group))
           (error (setq ebdb/gnus-score-rebuild-alist t)
                  (message "Problem building EBDB score table.")
                  (ding) (sit-for 2)
                  nil)))))

(defun ebdb/gnus-score-as-text (_group)
  "Returns a SCORE file format string built from the EBDB."
  (cond ((or (cond ((/= (or ebdb/gnus-score-default 0)
                        (or ebdb/gnus-score-default-internal 0))
                    (setq ebdb/gnus-score-default-internal
                          ebdb/gnus-score-default)
                    t))
             (not ebdb/gnus-score-alist)
             ebdb/gnus-score-rebuild-alist)
         (setq ebdb/gnus-score-rebuild-alist nil)
         (setq ebdb/gnus-score-alist
               (concat "((touched nil) (\"from\"\n"
                       (mapconcat
                        (lambda (record)
                          (let ((score (or (ebdb-record-user-field record 'ebdb-gnus-score-field)
                                           ebdb/gnus-score-default))
                                (mail (ebdb-record-mail record)))
                            (when (and score mail)
                              (mapconcat
                               (lambda (address)
                                 (format "(\"%s\" %s)\n" (ebdb-string address) score))
                               mail ""))))
                        ebdb-record-tracker "")
                       "))"))))
  ebdb/gnus-score-alist)

;;; from Brian Edmonds' gnus-ebdb.el
;;;
;;; Splitting / filing with gnus-folder
;;;
;;; To use this feature, you need to put this file somewhere in your
;;; load-path and add the following lines of code to your .gnus file:
;;;
;;; (setq nnmail-split-methods 'ebdb/gnus-split-method)
;;;
;;; You should also examine the variables defvar'd below and customize
;;; them to your taste.  They're listed roughly in descending likelihood
;;; of your wanting to change them.  Once that is done, you need to add
;;; filing information to your EBDB.  There are two fields of interest:
;;;
;;; 1. gnus-private.  This field contains the name of the group in which
;;;    mail to you from any of the addresses associated with this record
;;;    will be filed.  Also, any self-copies of mail you send any of the
;;;    same addresses will be filed here.
;;; 2. gnus-public.  This field is used to keep mail from mailing lists
;;;    out of the private mailboxes.  It should be added to a record for
;;;    the list submission address, and is formatted as follows:
;;;      "group regexp"
;;;    where group is where mail from the list should be filed, and
;;;    regexp is a regular expression which is checked against the
;;;    envelope sender (from the From_ header) to verify that this is
;;;    the copy which came from the list.  For example, the entry for
;;;    the ding mailing list might be:
;;;      "mail.emacs.ding ding-request@ifi.uio.no"
;;;    Yes, the second part *is* a regexp, so those dots may match
;;;    something other than dots.  Sue me.
;;;
;;; Note that you can also specify a gnus-private field for mailing list
;;; addresses, in which case self-copies of mail you send to the list
;;; will be filed there.  Also, the field names can be changed below if
;;; the defaults are not hip enough for you.  Lastly, if you specify a
;;; gnus-private field for your *own* EBDB record, then all self-copies
;;; of mail you send will be filed to that group.
;;;
;;; This documentation should probably be expanded and moved to a
;;; separate file, but it's late, and *I* know what I'm trying to
;;; say. :)

(defcustom ebdb/gnus-split-default-group "mail.misc"
  "If the EBDB does not indicate any group to spool a message to, it will
be spooled to this group.  If `ebdb/gnus-split-crosspost-default' is not
nil, and if the EBDB did not indicate a specific group for one or more
addresses, messages will be crossposted to this group in addition to any
group(s) which the EBDB indicated."
  :group 'ebdb-mua-gnus-splitting
  :type  'string)

(defcustom ebdb/gnus-split-nomatch-function nil
  "This function will be called after searching the EBDB if no place to
file the message could be found.  It should return a group name (or list
of group names) -- `nnmail-split-fancy' as provided with Gnus is an
excellent choice."
  :group 'ebdb-mua-gnus-splitting
  :type  'function)

(defcustom ebdb/gnus-split-myaddr-regexp
  (concat "^" (user-login-name) "$\\|^"
          (user-login-name) "@\\([-a-z0-9]+\\.\\)*"
          (or (message-make-domain) (system-name) "") "$")
  "This regular expression should match your address as found in the
From header of your mail."
  :group 'ebdb-mua-gnus-splitting
  :type  'regexp)

(defcustom ebdb/gnus-split-crosspost-default nil
  "If this variable is not nil, then if the EBDB could not identify a
group for every mail address, messages will be filed in
`ebdb/gnus-split-default-group' in addition to any group(s) which the EBDB
identified."
  :group 'ebdb-mua-gnus-splitting
  :type  'boolean)

;; The split function works by assigning one of four spooling priorities
;; to each group that is associated with an address in the message.  The
;; priorities are assigned as follows:
;;
;; 0. This priority is assigned when crosspost-default is nil to To/Cc
;;    addresses which have no private group defined in the EBDB.  If the
;;    user's own address has no private group defined, then it will
;;    always be given this priority.
;; 1. This priority is assigned to To/Cc addresses which have a private
;;    group defined in the EBDB.  If crosspost-default is not nil, then
;;    To/Cc addresses which have no private group will also be assigned
;;    this priority.  This is also assigned to the user's own address in
;;    the From position if a private group is defined for it.
;; 2. This priority is assigned to From addresses which have a private
;;    group defined in the EBDB, except for the user's own address as
;;    described under priorities 0 and 1.
;; 3. This priority is assigned to To/Cc addresses which have a public
;;    group defined in the EBDB, and whose associated regular expression
;;    matches the envelope sender (found in the header From_).
;;
;; The split function evaluates the spool priority for each address in
;; the headers of the message, and returns as a list all the groups
;; associated with the addresses which share the highest calculated
;; priority.

;;;###autoload
(defun ebdb/gnus-split-method ()
  "This function expects to be called in a buffer which contains a mail
message to be spooled, and the buffer should be narrowed to the message
headers.  It returns a list of groups to which the message should be
spooled, using the addresses in the headers and information from EBDB."
  (let ((prq (list (list 0) (list 1) (list 2) (list 3))))
    ;; the From: header is special
    (let* ((hdr (or (mail-fetch-field "resent-from")
                    (mail-fetch-field "from")
                    (user-login-name)))
           (rv (ebdb/gnus-split-to-group hdr t)))
      (setcdr (nth (cdr rv) prq) (list (car rv))))
    ;; do the rest of the headers
    (let ((hdr (or (concat (or (mail-fetch-field "resent-to" nil t)
                               (mail-fetch-field "to" nil t))
                           ", "
                           (mail-fetch-field "cc" nil t)
                           ", "
                           (mail-fetch-field "apparently-to" nil t))
                   "")))
      (dolist (address (ebdb-extract-address-components hdr t))
        (let* ((rv (ebdb/gnus-split-to-group address))
               (pr (nth (cdr rv) prq)))
          (unless (member-ignore-case (car rv) pr)
            (setcdr pr (cons (car rv) (cdr pr)))))))
    ;; find the highest non-empty queue
    (setq prq (reverse prq))
    (while (and prq (not (cdr (car prq)))) (setq prq (cdr prq)))
    ;; and return...
    (if (not (or (not (cdr (car prq)))
                 (and (equal (cdr (car prq)) (list ebdb/gnus-split-default-group))
                      (symbolp ebdb/gnus-split-nomatch-function)
                      (fboundp ebdb/gnus-split-nomatch-function))))
        (cdr (car prq))
      (goto-char (point-min))
      (funcall ebdb/gnus-split-nomatch-function))))

(defun ebdb/gnus-split-to-group (address &optional source)
  "This function is called from `ebdb/gnus-split-method' in order to
determine the group and spooling priority for a single address."
  (condition-case nil
      (let* ((tmp (ebdb-extract-address-components address))
             (mail (cadr tmp))
             (record (car (ebdb-message-search (car tmp) mail)))
             public private rgx)
        (when record
          (setq private (ebdb-record-user-field record  'ebdb-gnus-private-field)
		;; TODO: Fix this, there's no longer a public field.
                public (ebdb-record-user-field record 'ebdb/gnus-split-public-field))
          (if (and public (not source) (string-match "^\\([^ ]+\\) \\(.*\\)$" public))
              (setq rgx (substring public (match-beginning 2) (match-end 2))
                    public (substring public (match-beginning 1) (match-end 1)))
            (setq public nil)))
        (cond
         ((and rgx public
               (goto-char (point-min))
               (re-search-forward "^From: \\([^ \n]+\\)[ \n]" nil t)
               (string-match rgx (buffer-substring (match-beginning 1)
                                                   (match-end 1))))
          (cons public 3))
         (private
          (cons private
                (- 1 (if source -1 0)
                   (if (string-match ebdb/gnus-split-myaddr-regexp mail) 1 0))))
         (t
          (cons ebdb/gnus-split-default-group
                (cond ((string-match ebdb/gnus-split-myaddr-regexp mail) 0)
                      (source 2)
                      (ebdb/gnus-split-crosspost-default 1)
                      (t 0))))))
    (error (cons ebdb/gnus-split-default-group 0))))

;;
;; Imap support (Uwe Brauer)
;;

;; Why wouldn't we just use the same "private" field for imap? Why
;; would this need to be a separate field class?
(defclass ebdb-gnus-imap-field (ebdb-field-user)
  ((group
    :type string
    :initarg :group))
  :human-readable "gnus imap")

(cl-defmethod ebdb-read ((field (subclass ebdb/gnus-imap-field)) &optional slots obj)
  (let ((group (ebdb-read-string "Imap group: " (when obj (slot-value obj 'group)))))
    (cl-call-next-method
     field
     (plist-put slots :group group)
     obj)))

(cl-defmethod ebdb-string ((field ebdb-gnus-imap-field))
  (slot-value field 'group))

(defun ebdb/gnus-nnimap-folder-list-from-ebdb ()
  "Return a list of \( \"From\" mail-regexp imap-folder-name\) tuples
based on the contents of the ebdb.

The folder-name is the value of the 'imap attribute of the EBDB record;
the mail-regexp consists of all the mail addresses for the EBDB record
concatenated with OR.  Records without an 'imap attribute are ignored.

Here  is an example of a relevant EBDB record:

Uwe Brauer
           mail: oub@mat.ucm.es
           imap: testimap

This function uses `regexp-opt' to generate the mail-regexp which automatically
`regexp-quote's its arguments.  Please note: in order that this will work
with the `nnimap-split-fancy' method you have to use macros, that is your setting
will look like:

\(setq nnimap-split-rule  'nnimap-split-fancy
       nnimap-split-inbox \"INBOX\"
       nnimap-split-fancy
       `\(| ,@\(ebdb/gnus-nnimap-folder-list-from-ebdb\)
            ... \)\)

Note that `\( is the backquote, NOT the quote '\(."

  (let (;; the value of the 'imap attribute of a ebdb record
        folder-attr
        ;; a regexp matching all the mail addresses from a ebdb record
        mail-regexp
        ;; the list of (folder mail) tuples to return
        new-elmnt-list)
    ;; Loop over EBDB records.  If an imap attribute exists for
    ;; the record, generate a regexp matching all the mail addresses
    ;; and add a tuple (folder mail-regexp) to the new-elmnt-list
    (dolist (record (ebdb-records))
      (when (setq folder-attr (ebdb-record-user-field record 'imap))
        (setq mail-regexp (regexp-opt (mapcar (lambda (m)
						(downcase (ebdb-string m)))
                                              (ebdb-record-mail record))))
        (unless (string= "" mail-regexp)
          (push (list "From" mail-regexp folder-attr)
                new-elmnt-list))))
    new-elmnt-list))

;;
;; Insinuation
;;

(add-hook 'gnus-article-prepare-hook 'ebdb-mua-auto-update)

(add-hook 'gnus-startup-hook 'ebdb-insinuate-gnus)

(defsubst ebdb-gnus-buffer-name ()
  (format "*%s-Gnus*" ebdb-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode gnus-summary-mode))
  "Produce a EBDB buffer name associated with Gnus."
  (ebdb-gnus-buffer-name))

(cl-defmethod ebdb-make-buffer-name (&context (major-mode gnus-article-mode))
  "Produce a EBDB buffer name associated with Gnus."
  (ebdb-gnus-buffer-name))

(cl-defmethod ebdb-popup-window (&context (major-mode gnus-summary-mode))
  (let ((win
	 (progn
	   (unless (gnus-buffer-live-p gnus-article-buffer)
	     (gnus-summary-show-article))
	   (get-buffer-window gnus-article-buffer))))
    (list win 0.3)))

(cl-defmethod ebdb-popup-window (&context (major-mode gnus-article-mode))
  (list (get-buffer-window) 0.3))

;; It seems that `gnus-fetch-field' fetches decoded content of
;; `gnus-visible-headers', ignoring `gnus-ignored-headers'.
;; Here we use instead `gnus-fetch-original-field' that fetches
;; the encoded content of `gnus-original-article-buffer'.
;; Decoding makes this possibly a bit slower, but something like
;; `ebdb-select-message' does not get fooled by an apparent
;; absence of some headers.
;; See http://permalink.gmane.org/gmane.emacs.gnus.general/78741

(cl-defmethod ebdb-message-header ((header string)
				   &context (major-mode gnus-summary-mode))
  "Return value of HEADER for current Gnus message."
  (set-buffer gnus-article-buffer)
  (gnus-fetch-original-field header))

;; This is all a little goofy.
(cl-defmethod ebdb-message-header ((header string)
				   &context (major-mode gnus-article-mode))
  (set-buffer gnus-article-buffer)
  (gnus-fetch-original-field header))

(cl-defmethod ebdb-message-header ((header string)
				   &context (major-mode gnus-tree-mode))
  (set-buffer gnus-article-buffer)
  (gnus-fetch-original-field header))

(cl-defmethod ebdb-mua-prepare-article (&context (major-mode gnus-summary-mode))
  (gnus-summary-select-article))

(cl-defmethod ebdb-mua-prepare-article (&context (major-mode gnus-article-mode))
  (gnus-summary-select-article))

(cl-defmethod ebdb-mua-article-body (&context (major-mode gnus-summary-mode))
  "Return the current article body as a string.

Must not include article headers, though can include headers in
quoted replies."
  (gnus-with-article-buffer
    ;; This pretends that there's no such thing as mime parts, and
    ;; will probably fail horribly.
    (article-goto-body)
    (buffer-substring-no-properties (point) (point-max))))

(cl-defmethod ebdb-mua-article-body (&context (major-mode gnus-article-mode))
   (gnus-with-article-buffer
    (article-goto-body)
    (buffer-substring-no-properties (point) (point-max))))

(defun ebdb-insinuate-gnus ()
  "Hook EBDB into Gnus."
  ;; `ebdb-mua-display-sender' fails in *Article* buffers, where
  ;; `gnus-article-read-summary-keys' provides an additional wrapper
  ;; that restores the window configuration.
  (define-key gnus-summary-mode-map ";" 'ebdb-mua-update-records)
  (define-key gnus-article-mode-map ";" 'ebdb-mua-update-records)
  ;; For `ebdb-mua-edit-field-sender' it is probably OK if
  ;;`gnus-article-read-summary-keys' restores the window configuration.
  (define-key gnus-summary-mode-map ":" 'ebdb-mua-edit-field-sender)
  (define-key gnus-article-mode-map ":" 'ebdb-mua-edit-field-sender)
  ;; Do we need keybindings for more commands?  Suggestions welcome.
  ;; (define-key gnus-summary-mode-map ":" 'ebdb-mua-display-records)
  ;; (define-key gnus-summary-mode-map "'" 'ebdb-mua-display-recipients)
  ;; (define-key gnus-summary-mode-map ";" 'ebdb-mua-edit-field-recipients)

  ;; Set up user field for use in `gnus-summary-line-format'
  ;; (1) Big solution: use whole name
  (if ebdb-mua-summary-unify-format-letter
      (fset (intern (concat "gnus-user-format-function-"
                            ebdb-mua-summary-unify-format-letter))
            (lambda (header)
	      (let ((from (mail-header-from header)))
		(or
		 (and gnus-ignored-from-addresses
		      (cond ((functionp gnus-ignored-from-addresses)
			     (funcall gnus-ignored-from-addresses
				      (mail-strip-quoted-names from)))
			    (t (string-match (gnus-ignored-from-addresses) from)))
		      (let ((extra-headers (mail-header-extra header))
			    to
			    newsgroups)
			(cond
			 ((setq to (cdr (assq 'To extra-headers)))
			  (concat gnus-summary-to-prefix
				  (ebdb-mua-summary-unify to)))
			 ((setq newsgroups
				(or
				 (cdr (assq 'Newsgroups extra-headers))
				 (and
				  (memq 'Newsgroups gnus-extra-headers)
				  (eq (car (gnus-find-method-for-group
					    gnus-newsgroup-name)) 'nntp)
				  (gnus-group-real-name gnus-newsgroup-name))))
			  (concat gnus-summary-newsgroup-prefix newsgroups)))))
		 (ebdb-mua-summary-unify (mail-header-from header)))))))

  ;; (2) Small solution: a mark for messages whos sender is in EBDB.
  (if ebdb-mua-summary-mark-format-letter
      (fset (intern (concat "gnus-user-format-function-"
                            ebdb-mua-summary-mark-format-letter))
            (lambda (header)
              (ebdb-mua-summary-mark (mail-header-from header)))))

  ;; Scoring
;  (add-hook 'ebdb-after-change-hook 'ebdb/gnus-score-invalidate-alist)
  )
  ;; (setq gnus-score-find-score-files-function
  ;;  (if (boundp 'gnus-score-find-score-files-function)
  ;;      (cond ((functionp gnus-score-find-score-files-function)
  ;;             (list gnus-score-find-score-files-function 'ebdb/gnus-score))
  ;;            ((listp gnus-score-find-score-files-function)
  ;;             (append gnus-score-find-score-files-function 'ebdb/gnus-score))
  ;;            (t 'ebdb/gnus-score))
  ;;    'ebdb/gnus-score))

(provide 'ebdb-gnus)
;;; ebdb-gnus.el ends here
;;; 
