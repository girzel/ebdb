;;; ebdb-mua.el --- Mail user agent interaction for EBDB  -*- lexical-binding: t; -*-

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

;; This library handles EBDB interaction with various other packages.
;; The name implies integration with mail user agents, but in fact it
;; could be used for any package that wants EBDB pop-up integration.
;; For simplicity's sake, all these packages will be referred to as
;; MUAs.

;; Essentially, this library can make three things happen:

;; 1. Return EBDB records matched by criteria provided by the MUA, and
;; optionally display those records in a pop-up buffer.

;; 2. Handle information provided by the MUA which does not exactly
;; match the existing records.  This can mean creating new records,
;; and/or updating existing records based on new information about
;; names or messages, or running other user-defined rules.  These
;; updates may be automatic or interactive, depending on the user's
;; configuration.

;; 3. Provide keybindings for editing or otherwise manipulating the
;; records afterwards.

;;; Code:

(require 'ebdb)
(require 'ebdb-com)

(eval-and-compile
  (autoload 'mail-decode-encoded-word-string "mail-parse"))

;;; MUA interface

(defvar ebdb-offer-to-create nil
  "For communication between `ebdb-update-records' and `ebdb-query-create'.")

(defvar ebdb-update-records-p nil
  "For communication between `ebdb-update-records' and
  `ebdb-query-create'.")

(defvar ebdb-update-records-address nil
  "For communication between `ebdb-update-records' and `ebdb-query-create'.
It is a list with elements (NAME MAIL HEADER HEADER-CLASS MUA).")

(defcustom ebdb-annotate-field ebdb-default-user-field
  "Field to annotate via `ebdb-annotate-record' and friends.
This may take the values:
 affix           The list of affixes
 organization    The list of organizations
 aka             the list of AKAs
 mail            the list of email addresses
 all-fields      Read the field to edit using a completion table
                   that includes all fields currently known to EBDB.

Any other symbol is interpreted as the label of an xfield."
  :group 'ebdb-mua
  :type '(symbol :tag "Field to annotate"))

(defcustom ebdb-mua-edit-field ebdb-default-user-field
  "Field to edit with command `ebdb-mua-edit-field' and friends.
This may take the values:
 name            The full name
 affix           The list of affixes
 organization    The list of organizations
 aka             the list of AKAs
 mail            the list of email addresses
 all-fields      Read the field to edit using a completion table
                   that includes all fields currently known to EBDB.

Any other symbol is interpreted as the label of an xfield."
  :group 'ebdb-mua
  :type '(symbol :tag "Field to edit"))

(defcustom ebdb-mua-auto-update-p 'existing
  "This option governs how EBDB handles addresses found in
  incoming mail messages.  It can take one of the following
  values:

 nil          Do nothing.
 existing     Find existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 a function   This functions will be called with no arguments.
                It should return one of the above values.

Note that this option only controls how EBDB acts on information
in incoming messages; the option `ebdb-mua-pop-up' controls
whether the records in question are actually displayed or not."

  ;; Also: Used for communication between `ebdb-update-records'
  ;; and `ebdb-query-create'.
  :group 'ebdb-mua
  :type '(choice (const :tag "do nothing" nil)
                 (const :tag "search for existing records" search)
                 (const :tag "update existing records" update)
                 (const :tag "query annotation of all messages" query)
                 (const :tag "annotate all messages" create)
                 (function :tag "User-defined function")))

(defcustom ebdb-message-headers
  '((sender     "From" "Resent-From" "Reply-To" "Sender")
    (recipients "Resent-To" "Resent-CC" "To" "CC" "BCC"))
  "Alist of headers to search for sender and recipients mail addresses.
Each element is of the form

  (CLASS HEADER ...)

The symbol CLASS defines a class of headers.
The strings HEADER belong to CLASS."
  :group 'ebdb-mua
  :type 'list)

(defcustom ebdb-message-all-addresses nil
  "If t `ebdb-update-records' returns all mail addresses of a message.
Otherwise this function returns only the first mail address of each message."
  :group 'ebdb-mua
  :type 'boolean)

(defcustom ebdb-message-try-all-headers nil
  "If t try all message headers to extract an email address from a message.
Several EBDB commands extract either the sender or the recipients' email
addresses from a message according to `ebdb-message-headers'.  If EBDB does not
find any email address in this subset of message headers (for example, because
an email address is excluded because of `ebdb-user-mail-address-re')
but `ebdb-message-try-all-headers' is t, then these commands will also consider
the email addresses in the remaining headers."
  :group 'ebdb-mua
  :type 'boolean)

;; TODO: Handle more headers.  Why not make it possible for EBDB to
;; ignore all messages more than five years old, for instance?  Also,
;; there was originally a distinction between ignoring a message
;; (everything about the message), and ignoring an individual
;; name/mail element of a message.  We probably want to restore that
;; distinction.  Ie, we'll have a full complement of
;; `ebdb-(accept|ignore)-(message|address)-alist' variables.

(defcustom ebdb-accept-header-alist nil
  "Alist describing which messages to automatically create EBDB records for.
The format of this alist is
   ((HEADER-TYPE . REGEXP) ...)

Where HEADER-TYPE is one of the symbols 'sender, 'recipients',
'any (meaning 'sender or 'recipients), or 'subject.

For example, if
   ((sender . \"@.*\\.maximegalon\\.edu\")
    (subject . \"time travel\"))
EBDB records are only created for messages sent by people at Maximegalon U.,
or people posting about time travel.
If t accept all messages.  If nil, accept all messages.

See also `ebdb-ignore-message-alist', which has the opposite effect."
  :group 'ebdb-mua
  :type '(repeat (cons
                  (choice (symbol :tag "Sender" sender)
			  (symbol :tag "Recipients" recipients)
			  (symbol :tag "Sender or recipients" any)
			  (symbol :tag "Subject" subject))
                  (regexp :tag "Regexp to match on header value"))))

(defcustom ebdb-ignore-header-alist nil
  "Alist describing which messages not to automatically create EBDB records for.
The format of this alist is
   ((HEADER-TYPE . REGEXP) ... )

Where HEADER-TYPE is one of the symbols 'sender, 'recipients',
'any (meaning 'sender or 'recipients), or 'subject.

For example, if
   ((sender . \"mailer-daemon\")
    (recipients . \"mailing-list-1\\\\|mailing-list-2\"))
no EBDB records are created for messages from any mailer daemon,
or messages sent to or CCed to either of two mailing lists.
If t ignore all messages.  If nil do not ignore any messages.

See also `ebdb-accept-message-alist', which has the opposite effect."
  :group 'ebdb-mua
  :type '(repeat (cons
                  (choice (symbol :tag "Sender" sender)
			  (symbol :tag "Recipients" recipients)
			  (symbol :tag "Sender or recipients" any)
			  (symbol :tag "Subject" subject))
                  (regexp :tag "Regexp to match on header value"))))

(defcustom ebdb-user-mail-address-re
  (and (stringp user-mail-address)
       (string-match "\\`\\([^@]*\\)\\(@\\|\\'\\)" user-mail-address)
       (concat "\\<" (regexp-quote (match-string 1 user-mail-address)) "\\>"))
  "A regular expression matching your mail addresses.
This option can be directly set to a regexp.  It can also be the
symbol 'message, in which case the value of
`message-alternative-emails' will be used, or the symbol 'self,
in which case the value will be constructed from the mail
addresses of the record pointed to by `ebdb-record-self'.
Several EBDB commands extract either the sender or the
recipients' email addresses from a message according to
`ebdb-message-headers'.  Yet an email address will be ignored if
it matches `ebdb-user-mail-address-re'.  This way the commands
will not operate on your own record.  See also
`ebdb-message-try-all-headers'."
  :group 'ebdb-mua
  :type '(choice (const :tag "Use addresses from `ebdb-record-self'" self)
		 (const :tag "Use the value of `message-alternative-emails'" message)
		 (regexp :tag "Regexp matching your mail addresses")))

;; This is currently only called in `ebdb-mua-test-headers'.
(defun ebdb-get-user-mail-address-re ()
  "Get or set the value of variable `ebdb-user-mail-address-re'.

If it's a symbol, check if it's one of 'self or 'message, and set
accordingly."
  (cond ((stringp ebdb-user-mail-address-re)
	 ebdb-user-mail-address-re)
	((eq ebdb-user-mail-address-re 'self)
	 (let ((self-rec (ebdb-gethash ebdb-record-self 'uuid)))
	   (unless self-rec
	     (user-error "`ebdb-user-mail-address-re' set to 'self, but `ebdb-record-self' is not set."))
	   (setq ebdb-user-mail-address-re
		 (regexp-opt (slot-value
			      (ebdb-record-cache self-rec)
			      'mail-canon)))))
	((eq ebdb-user-mail-address-re 'message)
	 (setq ebdb-user-mail-address-re
	       message-alternative-emails))
	(t ebdb-user-mail-address-re)))

(defcustom ebdb-add-name 'query
  "How to handle new names for existing EBDB records.
This handles messages where the real name differs from the name
in a EBDB record with the same mail address, as in \"John Smith <jqs@frob.com>\"
versus \"John Q. Smith <jqs@frob.com>\".
Allowed values are:
 t           Automatically change the name to the new value.
 query       Query whether to use the new name.
 nil         Ignore the new name.
 a number    Number of seconds EBDB displays the name mismatch.
               (without further action).
 a function  This is called with two args, the record and the new name.
               It should return one of the above values.
 a regexp    If the new name matches this regexp ignore it.
               Otherwise query to add it.
See also `ebdb-add-aka'."
  :group 'ebdb-mua
  :type '(choice (const :tag "Automatically use the new name" t)
                 (const :tag "Query for name changes" query)
                 (const :tag "Ignore the new name" nil)
                 (integer :tag "Number of seconds to display name mismatch")
                 (function :tag "Function for analyzing name handling")
                 (regexp :tag "If the new name matches this regexp ignore it.")))

(defcustom ebdb-add-aka 'query
  "How to handle alternate names for existing EBDB records.
Allowed values are:
 t           Automatically store alternate names as AKA.
 query       Query whether to store alternate names as an AKA.
 nil         Ignore alternate names.
 a function  This is called with two args, the record and the new name.
               It should return one of the above values.
 a regexp    If the alternate name matches this regexp ignore it.
               Otherwise query to add it.
See also `ebdb-add-name'."
  :group 'ebdb-mua
  :type '(choice (const :tag "Automatically store alternate names as AKA" t)
                 (const :tag "Query for alternate names" query)
                 (const :tag "Ignore alternate names" nil)
                 (function :tag "Function for alternate name handling")
                 (regexp :tag "If the alternate name matches this regexp ignore it.")))

(defcustom ebdb-add-mails 'query
  "How to handle new mail addresses for existing EBDB records.
This handles messages where the mail address differs from the mail addresses
in a EBDB record with the same name as in \"John Q. Smith <jqs@foo.com>\"
versus \"John Q. Smith <jqs@bar.com>\".
Allowed values are:
 t           Automatically add new mail addresses to the list of mail addresses.
 query       Query whether to add it.
 nil         Ignore new mail addresses.
 a number    Number of seconds EBDB displays the new address
               (without further action).
 a function  This is called with two args, the record and the new mail address.
               It should return one of the above values.
 a regexp    If the new mail address matches this regexp ignore the new address.
               Otherwise query to add it.
See also `ebdb-ignore-redundant-mails'."
  :group 'ebdb-mua
  :type '(choice (const :tag "Automatically add new mail addresses" t)
                 (const :tag "Query before adding new mail addresses" query)
                 (const :tag "Never add new mail addresses" nil)
                 (number :tag "Number of seconds to display new addresses")
                 (function :tag "Function for analyzing name handling")
                 (regexp :tag "If the new address matches this regexp ignore it.")))

(defcustom ebdb-canonicalize-mail-function nil
  "If non-nil, it should be a function of one arg: a mail address string.
When EBDB \"notices\" a message, the corresponding mail addresses are passed
to this function first.  It acts as a kind of \"filter\" to transform
the mail addresses before they are compared against or added to the database.
See `ebdb-canonicalize-mail-1' for a more complete example.
If this function returns nil, EBDB assumes that there is no mail address.

See also `ebdb-ignore-redundant-mails'."
  :group 'ebdb-mua
  :type 'function)

(defcustom ebdb-ignore-redundant-mails 'query
  "How to handle redundant mail addresses for existing EBDB records.
For example, \"foo@bar.baz.com\" is redundant w.r.t. \"foo@baz.com\".
This affects two things, whether a new redundant mail address is added
to EBDB and whether an old mail address, which has become redundant
because of a newly added mail address, is removed from EBDB.

Allowed values are:
 t           Automatically ignore redundant mail addresses.
 query       Query whether to ignore them.
 nil         Do not ignore redundant mail addresses.
 a number    Number of seconds EBDB displays redundant mail addresses
               (without further action).
 a function  This is called with two args, the record and the new mail address.
               It should return one of the above values.
 a regexp    If the new mail address matches this regexp never ignore
               this mail address.  Otherwise query to ignore it.
See also `ebdb-add-mails' and `ebdb-canonicalize-mail-function'."
  :group 'ebdb-mua
  :type '(choice (const :tag "Automatically ignore redundant mail addresses" t)
                 (const :tag "Query whether to ignore them" query)
                 (const :tag "Do not ignore redundant mail addresses" nil)
                 (number :tag "Number of seconds to display redundant addresses")
                 (function :tag "Function for handling redundant mail addresses")
                 (regexp :tag "If the new address matches this regexp never ignore it.")))
(define-obsolete-variable-alias 'ebdb-canonicalize-redundant-mails
  'ebdb-ignore-redundant-mails)

(defcustom ebdb-message-clean-name-function 'ebdb-message-clean-name-default
  "Function to clean up the name in the header of a message.
It takes one argument, the name as extracted by
`mail-extract-address-components'."
  :group 'ebdb-mua
  :type 'function)

(defcustom ebdb-message-mail-as-name t
  "If non-nil use mail address of message as fallback for name of new records."
  :group 'ebdb-mua
  :type 'boolean)

(defcustom ebdb-notice-mail-hook nil
  "Hook run each time a mail address of a record is \"noticed\" in a message.
This means that the mail address in a message belongs to an existing EBDB record
or to a record EBDB has created for the mail address.

Run with one argument, the record.  It is up to the hook function
to determine which MUA is used and to act appropriately.
Hook functions can use the variable `ebdb-update-records-address'
to determine the header and class of the mail address according
to `ebdb-message-headers'.  See `ebdb-auto-notes' for how to annotate records
using `ebdb-update-records-address' and the headers of a mail message.

If a message contains multiple mail addresses belonging to one EBDB record,
this hook is run for each mail address.  Use `ebdb-notice-record-hook'
if you want to notice each record only once per message."
  :group 'ebdb-mua
  :type 'hook)

(define-widget 'ebdb-alist-with-header 'group
  "My group"
  :match 'ebdb-alist-with-header-match
  :value-to-internal (lambda (_widget value)
                       (if value (list (car value) (cdr value))))
  :value-to-external (lambda (_widget value)
                       (if value (append (list (car value)) (cadr value)))))

(defun ebdb-alist-with-header-match (widget value)
  (widget-group-match widget
                      (widget-apply widget :value-to-internal value)))

(defvar ebdb-auto-notes-rules-expanded nil
  "Expanded `ebdb-auto-notes-rules'.") ; Internal variable

(defcustom ebdb-auto-notes-rules nil
  "List of rules for adding notes to records of mail addresses of messages.
This automatically annotates the EBDB record of the sender or recipient
of a message based on the value of a header such as the Subject header.
This requires that `ebdb-notice-mail-hook' contains `ebdb-auto-notes'
and that the record already exists or `ebdb-update-records-p' returns such that
the record will be created.  Messages matching `ebdb-auto-notes-ignore-messages'
are ignored.

The elements of this list are

   (FROM-TO HEADER ANNOTATE ...)
   (HEADER ANNOTATE ...)

FROM-TO is a list of headers and/or header classes as in `ebdb-message-headers'.
The record corresponding to a mail address of a message is considered for
annotation if this mail address was found in a header matching FROM-TO.
If FROM-TO is missing or t, records for each mail address are considered
irrespective of where the mail address was found in a message.

HEADER is a message header that is considered for generating the annotation.

ANNOTATE may take the following values:

  (REGEXP . STRING)       [this is equivalent to (REGEXP notes STRING)]
  (REGEXP FIELD STRING)
  (REGEXP FIELD STRING REPLACE)

REGEXP must match the value of HEADER for generating an annotation.
However, if the value of HEADER also matches an element of
`ebdb-auto-notes-ignore-headers' no annotation is generated.

The annotation will be added to FIELD of the respective record.
FIELD defaults to `ebdb-default-xfield'.

STRING defines a replacement for the match of REGEXP in the value of HEADER.
It may contain \\& or \\N specials used by `replace-match'.
The resulting string becomes the annotation.
If STRING is an integer N, the Nth matching subexpression is used.
If STRING is a function, it will be called with one arg, the value of HEADER.
The return value (which must be a string) is then used.

If REPLACE is t, the resulting string replaces the old contents of FIELD.
If it is nil, the string is appended to the contents of FIELD (unless the
annotation is already part of the content of field).

For example,

   ((\"To\" (\"-vm@\" . \"VM mailing list\"))
    (\"Subject\" (\"sprocket\" . \"mail about sprockets\")
               (\"you bonehead\" . \"called me a bonehead\")))

will cause the text \"VM mailing list\" to be added to the notes field
of the records corresponding to anyone you get mail from via one of the VM
mailing lists.

If multiple clauses match the message, all of the corresponding strings
will be added.

See also variables `ebdb-auto-notes-ignore-messages' and
`ebdb-auto-notes-ignore-headers'.

For speed-up, the function `ebdb-auto-notes' actually use expanded rules
stored in the internal variable `ebdb-auto-notes-rules-expanded'.
If you change the value of `ebdb-auto-notes-rules' outside of customize,
set `ebdb-auto-notes-rules-expanded' to nil, so that the expanded rules
will be re-evaluated."
  :group 'ebdb-mua
  :set (lambda (symbol value)
         (set-default symbol value)
         (setq ebdb-auto-notes-rules-expanded nil))
  :type '(repeat
          (ebdb-alist-with-header
           (repeat (choice
                    (const sender)
                    (const recipients)))
           (string :tag "Header name")
           (repeat (choice
                    (cons :tag "Value Pair"
                          (regexp :tag "Regexp to match on header value")
                          (string :tag "String for notes if regexp matches"))
                    (list :tag "Replacement list"
                          (regexp :tag "Regexp to match on header value")
                          (choice :tag "Record field"
                                  (const notes :tag "xfields")
                                  (const organization :tag "Organization")
                                  (symbol :tag "Other"))
                          (choice :tag "Regexp match"
                                  (string :tag "Replacement string")
                                  (integer :tag "Subexpression match")
                                  (function :tag "Callback Function"))
                          (choice :tag "Replace previous contents"
                                  (const :tag "No" nil)
                                  (const :tag "Yes" t))))))))

(defcustom ebdb-auto-notes-ignore-messages nil
  "List of rules for ignoring entire messages in `ebdb-auto-notes'.
The elements may have the following values:
  a function  This function is called with one arg, the record
              that would be annotated.
              Ignore this message if the function returns non-nil.
              This function may use `ebdb-update-records-address'.
  (HEADER . REGEXP)  Ignore messages where HEADER matches REGEXP.
              For example,  (\"From\" . ebdb-user-mail-address-re)
              disables any recording of notes for mail addresses
              found in messages coming from yourself, see
              `ebdb-user-mail-address-re'.
See also `ebdb-auto-notes-ignore-headers'."
  :group 'ebdb-mua
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regexp to match on header value"))))

(defcustom ebdb-auto-notes-ignore-headers nil
  "Alist of headers and regexps to ignore in `ebdb-auto-notes'.
Each element is of the form

    (HEADER . REGEXP)

For example,

    (\"Organization\" . \"^Gatewayed from\\\\\|^Source only\")

will exclude the phony `Organization:' headers in GNU mailing-lists
gatewayed to gnu.* newsgroups.
See also `ebdb-auto-notes-ignore-messages'."
  :group 'ebdb-mua
  :type '(repeat (cons
                  (string :tag "Header name")
                  (regexp :tag "Regexp to match on header value"))))

(defcustom ebdb-mua-pop-up t
  "If non-nil, display an auto-updated EBDB window while using a MUA."
  :group 'ebdb-mua
  :type 'boolean)



(defcustom ebdb-mua-summary-unification-list
  '(name mail message-name message-mail message-address)
  "List of FIELDs considered by `ebdb-mua-summary-unify'.
For the RECORD matching the address of a message, `ebdb-mua-summary-unify'
returns the first non-empty field value matching an element FIELD from this list.
Each element FIELD may be a valid argument of `ebdb-record-field' for RECORD.
In addition, this list may also include the following elements:
  message-name     The name in the address of the message
  message-mail     The mail in the address of the message
  message-address  The complete address of the message
These provide a fallback if a message does not have a matching RECORD
or if some FIELD of RECORD is empty."
  :group 'ebdb-mua
  :type '(repeat (symbol :tag "Field")))

;; There are two ways to customize the mark shown for a record in an
;; MUA's summary buffer.  One is to give the record an
;; `ebdb-field-summary-mark' field, holding the character to be
;; displayed.  The other is to implement the
;; `ebdb-mua-make-summary-mark' method, which accepts the record as an
;; argument, and returns a one-character string.  If both are present,
;; the per-record field wins.

(defclass ebdb-field-summary-mark (ebdb-field-user)
  ((char
    :type character
    :initarg :char
    :documentation
    "The character to display in MUA summary buffers for this
  record."))
  :human-readable "summary mark"
  :documentation "Field holding the character to be displayed in MUA summary
  buffers.")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-summary-mark)) &optional slots obj)
  (let ((char (read-char "Character: ")))
    (cl-call-next-method class (plist-put slots :char char) obj)))

(cl-defmethod ebdb-string ((field ebdb-field-summary-mark))
  (char-to-string (slot-value field 'char)))

(cl-defgeneric ebdb-mua-make-summary-mark (record)
  "Return a single-character string to mark RECORD in an MUA
  summary buffer.")

(cl-defmethod ebdb-mua-make-summary-mark ((_record ebdb-record))
  "By default, do nothing."
  nil)

(defcustom ebdb-mua-summary-mark "+"
  "Default mark for message addresses known to EBDB.
If nil do not mark message addresses known to EBDB.
See `ebdb-mua-summary-mark' and `ebdb-mua-summary-unify'.
See also the field class `ebdb-field-summary-mark'."
  :group 'ebdb-mua
  :type '(choice (string :tag "Mark used")
                 (const :tag "Do not mark known posters" nil)))

(defcustom ebdb-mua-summary-unify-format-letter "E"
  "Letter required for `ebdb-mua-summary-unify' in the MUA Summary format string.
For Gnus, combine it with the %u specifier in `gnus-summary-line-format'
\(see there), for example use \"%U%R%z%I%(%[%4L: %-23,23uB%]%) %s\\n\".
For VM, combine it with the %U specifier in `vm-summary-format' (see there),
for example, use \"%n %*%a %-17.17UB %-3.3m %2d %4l/%-5c %I\\\"%s\\\"\\n\".
This customization of `gnus-summary-line-format' / `vm-summary-format'
is required to use `ebdb-mua-summary-unify'.
Currently no other MUAs support this EBDB feature."
  :group 'ebdb-mua
  :type 'string)

(defcustom ebdb-mua-summary-mark-format-letter "e"
  "Letter required for `ebdb-mua-summary-mark' in the MUA Summary format string.
For Gnus, combine it with the %u specifier in `gnus-summary-line-format'
\(see there), for example, use \"%U%R%z%I%(%[%4L: %ue%-23,23f%]%) %s\\n\".
For VM, combine it with the %U specifier in `vm-summary-format' (see there),
for example, use \"%n %*%a %Ue%-17.17F %-3.3m %2d %4l/%-5c %I\\\"%s\\\"\\n\".
This customization of `gnus-summary-line-format' / `vm-summary-format'
is required to use `ebdb-mua-summary-mark'.
Currently no other MUAs support this EBDB feature."
  :group 'ebdb-mua
  :type 'string)

(defsubst ebdb-message-header-re (header regexp)
  "Return non-nil if REGEXP matches value of HEADER."
  (let ((val (ebdb-message-header header))
        (case-fold-search t)) ; RW: Is this what we want?
    (when (and val (string-match regexp val))
      (throw 'done t))))

(defun ebdb-mua-test-headers (header-type address-parts &optional ignore-address)
  "Decide if the address in ADDRESS-PARTS should be ignored or
  acted upon.  Return t if the header \"passes\".

Takes into consideration the IGNORE-ADDRESS argument, as well the
variables `ebdb-user-mail-address-re',
`ebdb-accept-header-alist', and `ebdb-ignore-header-alist'."
  (let ((name (car address-parts))
	(mail (cadr address-parts)))
    (and (null (or (and (stringp ignore-address)
			(or (and name (string-match-p ignore-address name))
			    (and mail (string-match-p ignore-address mail))))
		   (string-match-p
		    (ebdb-get-user-mail-address-re)
		    mail)))
	 (cond ((null (or ebdb-accept-header-alist
			  ebdb-ignore-header-alist))
		t)
	       ((and ebdb-accept-header-alist
		     (null ebdb-ignore-header-alist))
		(ebdb-mua-check-header header-type address-parts))
	       ((and ebdb-ignore-header-alist
		     (null ebdb-accept-header-alist))
		(ebdb-mua-check-header header-type address-parts t))
	       ((and ebdb-accept-header-alist
		     ebdb-ignore-header-alist)
		(and (ebdb-mua-check-header header-type address-parts)
		     (ebdb-mua-check-header header-type address-parts t)))))))

(defsubst ebdb-mua-check-header (header-type address-parts &optional invert)
  (let ((rest (if invert
		  ebdb-ignore-header-alist
		ebdb-accept-header-alist))
	h-type)
    (catch 'done
      (dolist (elt rest)
	(setq h-type (car elt))
	(cond ((and (eq h-type 'subject)
		    (eq header-type 'subject))
	       (when (ebdb-message-header-re "Subject" (cdr elt))
		 (throw 'done (if invert nil t))))
	      ((or (eq h-type header-type)
		   (and (eq h-type 'any)
			(memq header-type '(sender recipient))))
	       (when (string-match-p (cdr elt) (second address-parts))
		 (throw 'done (if invert nil t))))))
      (throw 'done t))))

;; How are you supposed to do the &context arglist for a defgeneric?
;; (cl-defgeneric ebdb-message-header (header)
;;   "Get value of HEADER for the mua keyed to major-mode.")

(defun ebdb-get-address-components (&optional header-class ignore-address)
  "Process mail addresses extracted from a message.
Return list with elements (NAME EMAIL HEADER HEADER-CLASS MUA).
HEADER-CLASS is defined in `ebdb-message-headers'.  If
HEADER-CLASS is nil, use all classes in `ebdb-message-headers'.

Returned address components are checked against the the values of
IGNORE-ADDRESS, `ebdb-user-mail-address-re',
`ebdb-accept-header-alist' and `ebdb-ignore-header-alist', and
are discarded as appropriate."
  ;; We do not use `ebdb-message-all-addresses' here because only when we
  ;; have compared the addresses with the records in EBDB do we know which
  ;; address(es) are relevant for us.
  (let ((message-headers (if header-class
                             (list (assoc header-class ebdb-message-headers))
                           ebdb-message-headers))
        address-list mail mail-list content)
    (condition-case nil
	(dolist (headers message-headers)
	  (dolist (header (cdr headers))
	    (when (setq content (ebdb-message-header header))
	      (setq content (mail-decode-encoded-word-string content))
	      (dolist (address (ebdb-extract-address-components content t))
		(setq mail (cadr address))
		;; Ignore addresses that should be ignored.
		(when (and mail
			   (not (member-ignore-case mail mail-list))
			   (ebdb-mua-test-headers (car headers) address ignore-address))
		  ;; Add each address only once. (Use MAIL-LIST for book keeping.)
		  ;; Thus if we care about whether an address gets associated with
		  ;; one or another header, the order of elements in
		  ;; `ebdb-message-headers' is relevant.  The "most important"
		  ;; headers should be first in `ebdb-message-headers'.
		  (push mail mail-list)
		  (push (list (car address) (cadr address) header (car headers) major-mode) address-list))))))
      (cl-no-applicable-method
       ;; Potentially triggered by `ebdb-message-header', which
       ;; dispatches on major-mode.
       (error "EBDB does not support %s" major-mode)))
    (or (nreverse address-list)
        (and header-class ebdb-message-try-all-headers
             ;; Try again the remaining header classes
             (let ((ebdb-message-headers
                    (remove (assoc header-class ebdb-message-headers)
                            ebdb-message-headers)))
               (ebdb-get-address-components nil ignore-address))))))

;;;###autoload
(defun ebdb-update-records (address-list &optional update-p sort)
  "Return the list of EBDB records matching ADDRESS-LIST.
ADDRESS-LIST is a list of mail addresses.  (It can be extracted from
a mail message using `ebdb-get-address-components'.)
UPDATE-P may take the following values:
 existing     Find existing records matching ADDRESS.
 update       Search for existing records matching ADDRESS;
                update name and mail field if necessary.
 query        Search for existing records matching ADDRESS;
                query for creation of a new record if the record does not exist.
 create or t  Search for existing records matching ADDRESS;
                create a new record if it does not yet exist.
 nil          Do nothing.
 a function   This functions will be called with no arguments.
                It should return one of the above values.

If SORT is non-nil, sort records according to `ebdb-record-lessp'.
Otherwise, the records are ordered according to ADDRESS-LIST.

Usually this function is called by the wrapper `ebdb-mua-auto-update'."

  (when (functionp update-p)
    (setq update-p (funcall update-p)))
  (when (eq t update-p)
    (setq update-p 'create))

  (let ( ;; `ebdb-update-records-p' and `ebdb-offer-to-create' are used here
        ;; as internal variables for communication with `ebdb-query-create'.
        ;; This does not affect the value of the global user variable
        ;; `ebdb-mua-auto-update-p'.
        (ebdb-offer-to-create 'start)
        (ebdb-update-records-p update-p)
        address records)

    (when update-p
      (while (setq address (pop address-list))
	(let* ((ebdb-update-records-address address)
	       hits
	       (task
		(catch 'done
		  (setq hits
			;; We put the call of `ebdb-notice-mail-hook'
			;; into `ebdb-annotate-message' so that this hook
			;; runs only if the user agreed to change a record.
			(cond ((eq ebdb-update-records-p 'existing)
			       ;; Search for records having this mail address
			       ;; but do not modify an existing record.
			       ;; This does not run `ebdb-notice-mail-hook'.
			       (ebdb-message-search (car address)
						    (cadr address)))
			      ((eq ebdb-update-records-p 'query)
			       (ebdb-annotate-message
				address 'ebdb-query-create))
			      (t
			       (ebdb-annotate-message
				address ebdb-update-records-p))))
		  nil)))
	  (cond ((eq task 'quit)
		 (setq address-list nil))
		((not (eq task 'next))
		 (dolist (hit (delq nil (nreverse hits)))
		   (cl-pushnew hit records :test #'equal))))
	  (if (and records (not ebdb-message-all-addresses))
	      (setq address-list nil))))
      (setq records
            (if sort (sort records 'ebdb-record-lessp)
              ;; Make RECORDS a list ordered like ADDRESS-LIST.
              (nreverse records))))

    ;; `ebdb-message-search' might yield multiple records
    (if (and records (not ebdb-message-all-addresses))
        (setq records (list (car records))))

    (dolist (record records)
      (ebdb-notice record))

    records))

;;; This whole thing could probably be replaced by `map-y-or-n-p'
(defun ebdb-query-create ()
  "Interactive query used by `ebdb-update-records'.
Return t if the record should be created or `nil' otherwise.
Honor previous answers such as `!'."
  (let ((task ebdb-offer-to-create))
    ;; If we have remembered what the user typed previously,
    ;; `ebdb-offer-to-create' holds a character, i.e., a number.
    ;; -- Right now, we only remember "!".
    (when (not (integerp task))
      (let ((prompt (format "%s is not in EBDB; add? (y,!,n,s,q,?) "
                            (or (nth 0 ebdb-update-records-address)
                                (nth 1 ebdb-update-records-address))))
            event)
        (while (not event)
          (setq event (read-key-sequence prompt))
          (setq event (if (stringp event) (aref event 0))))
        (setq task event)
        (message ""))) ; clear the message buffer

    (cond ((eq task ?y)
           t)
          ((eq task ?!)
           (setq ebdb-offer-to-create task)
           t)
          ((or (eq task ?n)
               (eq task ?\s))
           (throw 'done 'next))
          ((or (eq task ?q)
               (eq task ?\a)) ; ?\a = C-g
           (throw 'done 'quit))
          ((eq task ?s)
           (setq ebdb-update-records-p 'existing)
           (throw 'done 'next))
          (t ; any other key sequence
           (save-window-excursion
             (let* ((buffer (get-buffer-create " *EBDB Help*"))
                    (window (or (get-buffer-window buffer)
                                (split-window (get-lru-window)))))
               (with-current-buffer buffer
                 (special-mode)
                 (let (buffer-read-only)
                   (erase-buffer)
                   (insert
                    "Your answer controls how EBDB updates/searches for records.

Type ?  for this help.
Type y  to add the current record.
Type !  to add all remaining records.
Type n  to skip the current record. (You might also type space)
Type s  to switch from annotate to search mode.
Type q  to quit updating records.  No more search or annotation is done.")
                   (set-buffer-modified-p nil)
                   (goto-char (point-min)))
                 (set-window-buffer window buffer)
                 (fit-window-to-buffer window)))
             ;; Try again!
             (ebdb-query-create))))))



(defun ebdb-annotate-message (address &optional update-p)
  "Fill the records for message ADDRESS with as much info as possible.
If a record for ADDRESS does not yet exist, UPDATE-P controls whether
a new record is created for ADDRESS.  UPDATE-P may take the values:
 update or nil  Update existing records, never create a new record.
 query          Query interactively whether to create a new record.
 create or t    Create a new record.
 a function     This functions will be called with no arguments.
                  It should return one of the above values.
Return the records matching ADDRESS or nil."
  (let* ((mail (nth 1 address))		; possibly nil
         (name (unless (or (equal mail (car address))
			   (null (car address)))
                 (ebdb-string (ebdb-parse ebdb-default-name-class (car address)))))
         (records (ebdb-message-search name mail))
         created-p new-records)
    (if (and (not records) (functionp update-p))
        (setq update-p (funcall update-p)))
    (cond ((eq t update-p) (setq update-p 'create))
          ((not update-p) (setq update-p 'update)))

    ;; Create a new record if nothing else fits.
    ;; In this way, we can fill the slots of the new record with
    ;; the same code that updates the slots of existing records.
    (unless (or records
                (eq update-p 'update)
                (not (or name mail)))
      ;; If there is no name, try to use the mail address as name
      (if (and ebdb-message-mail-as-name mail
               (or (null name)
                   (string= "" name)))
          (setq name (funcall ebdb-message-clean-name-function mail)))
      (if (or (eq update-p 'create)
              (and (eq update-p 'query)
                   (y-or-n-p (format "%s is not in the EBDB.  Add? "
                                     (or name mail)))))
          (setq records (list (ebdb-db-add-record
			       (car ebdb-db-list)
			       (make-instance
				(slot-value (car ebdb-db-list) 'record-class))))
                created-p t)))

    (dolist (record records)
      (let* ((old-name (ebdb-record-name record))
             (fullname (ebdb-divide-name (or name "")))
             (fname (car fullname))
             (lname (cdr fullname))
             (mail mail) ;; possibly changed below
             (created-p created-p)
             (update-p update-p)
             change-p add-mails add-name ignore-redundant)

        ;; Analyze the name part of the record.
        (cond ((or (not name)
                   ;; The following tests can differ for more complicated names
                   (ebdb-string= name old-name)
                   (and (equal fname (ebdb-record-firstname record)) ; possibly
                        (equal lname (ebdb-record-lastname record))) ; nil
                   (member-ignore-case name (ebdb-record-aka record)))) ; do nothing

              (created-p ; new record
               (ebdb-record-change-name
		record
		(make-instance ebdb-default-name-class
			       :surname lname
			       :given-names (when fname (list fname)))))

              ((not (setq add-name (ebdb-add-job ebdb-add-name record name)))) ; do nothing

              ((numberp add-name)
               (unless ebdb-silent
                 (message "name mismatch: \"%s\" changed to \"%s\""
                          old-name name)
                 (sit-for add-name)))

              ((ebdb-eval-spec add-name
                               (if old-name
                                   (format "Change name \"%s\" to \"%s\"? "
                                           old-name name)
                                 (format "Assign name \"%s\" to address \"%s\"? "
                                         name (car (ebdb-record-mail record)))))
               ;; Keep old-name as AKA?
               (when (and old-name
                          (not (member-ignore-case old-name (ebdb-record-aka record))))
                 (if (ebdb-eval-spec (ebdb-add-job ebdb-add-aka record old-name)
                                     (format "Keep name \"%s\" as an AKA? " old-name))
                     (ebdb-record-set-field
                      record 'aka (cons old-name (ebdb-record-aka record)))
                   (ebdb-remhash old-name record)))
               (ebdb-record-set-field record 'name (cons fname lname))
               (setq change-p 'name))

              ;; make new name an AKA?
              ((and old-name
                    (not (member-ignore-case name (ebdb-record-aka record)))
                    (ebdb-eval-spec (ebdb-add-job ebdb-add-aka record name)
                                    (format "Make \"%s\" an alternate for \"%s\"? "
                                            name old-name)))
               (ebdb-record-set-field
                record 'aka (cons name (ebdb-record-aka record)))
               (setq change-p 'name)))

        ;; Is MAIL redundant compared with the mail addresses
        ;; that are already known for RECORD?
        (if (and mail
                 (setq ignore-redundant
                       (ebdb-add-job ebdb-ignore-redundant-mails record mail)))
            (let ((mails (ebdb-record-mail-canon record))
                  (case-fold-search t) redundant ml re)
              (while (setq ml (pop mails))
                (if (and (setq re (ebdb-mail-redundant-re ml))
                         (string-match re mail))
                    (setq redundant ml mails nil)))
              (if redundant
                  (cond ((numberp ignore-redundant)
                         (unless ebdb-silent
                           (message "%s: redundant mail `%s'"
                                    (ebdb-string record) mail)
                           (sit-for ignore-redundant)))
                        ((or (eq t ignore-redundant)
                             ebdb-silent
                             (y-or-n-p (format "Ignore redundant mail %s?" mail)))
                         (setq mail redundant))))))
	(setq mail (make-instance ebdb-default-mail-class :mail mail))
        ;; Analyze the mail part of the new records
        (cond ((or (not mail) (equal (ebdb-string mail) "???")
                   (member-ignore-case (ebdb-string mail) (ebdb-record-mail-canon record)))) ; do nothing

              (created-p		; new record
               (ebdb-record-insert-field record 'mail (list mail)))

              ((not (setq add-mails (ebdb-add-job ebdb-add-mails record mail)))) ; do nothing

              ((numberp add-mails)
               (unless ebdb-silent
                 (message "%s: new address `%s'"
                          (ebdb-string record) (ebdb-string mail))
                 (sit-for add-mails)))

              ((or (eq add-mails t)	; add it automatically
                   ebdb-silent
                   (y-or-n-p (format "Add address \"%s\" to %s? " (ebdb-string mail)
                                     (ebdb-string record)))
                   (and (or (and (functionp update-p)
                                 (progn (setq update-p (funcall update-p)) nil))
                            (memq update-p '(t create))
                            (and (eq update-p 'query)
                                 (y-or-n-p
                                  (format "Create a new record for %s? "
                                          (ebdb-string record)))))
                        (progn
                          (setq record (make-instance ebdb-default-record-class))
			  (ebdb-db-add-record (car ebdb-db-list) record)
                          (ebdb-record-change-name
			   record
			   (make-instance ebdb-default-name-class
					  :given-names (list fname)
					  :surname lname))
                          (setq created-p t))))

               (let ((mails (ebdb-record-mail record)))
                 (if ignore-redundant
                     ;; Does the new address MAIL make an old address redundant?
                     (let ((mail-re (ebdb-mail-redundant-re (ebdb-string mail)))
                           (case-fold-search t) okay redundant)
                       (dolist (ml mails)
                         (if (string-match mail-re (ebdb-string ml)) ; redundant mail address
                             (push ml redundant)
                           (push ml okay)))
                       (let ((form (format "redundant mail%s %s"
                                           (if (< 1 (length redundant)) "s" "")
                                           (ebdb-concat 'mail (nreverse redundant))))
                             (name (ebdb-record-name record)))
                         (if redundant
                             (cond ((numberp ignore-redundant)
                                    (unless ebdb-silent
                                      (message "%s: %s" name form)
                                      (sit-for ignore-redundant)))
                                   ((or (eq t ignore-redundant)
                                        ebdb-silent
                                        (y-or-n-p (format "Delete %s: " form)))
                                    (if (eq t ignore-redundant)
                                        (message "%s: deleting %s" name form))
                                    (setq mails okay)))))))

                 ;; then modify RECORD

		 ;; TODO: Reinstate the question about making this primary.
                 (ebdb-record-insert-field record 'mail mail)
                 (unless change-p (setq change-p t)))))

        (cond (created-p
               (unless ebdb-silent
                 (if (ebdb-record-name record)
                     (message "created %s's record with address \"%s\""
                              (ebdb-string record) mail)
                   (message "created record with naked address \"%s\"" mail)))
               (ebdb-init-record record))

              (change-p
               (unless ebdb-silent
                 (cond ((eq change-p 'name)
                        (message "noticed \"%s\"" (ebdb-string record)))
                       ((ebdb-record-name record)
                        (message "noticed %s's address \"%s\""
                                 (ebdb-string record) mail))
                       (t
                        (message "noticed naked address \"%s\"" mail))))))

        ;;(run-hook-with-args 'ebdb-notice-mail-hook record)
	;; (ebdb-notice record) ; I think this is already happening in
	;; `ebdb-update-records'.
        (push record new-records)))

    (nreverse new-records)))

(cl-defmethod ebdb-mua-prepare-article ()
  "Do whatever preparations are necessary to work on records
  associated with the current message.

Dispatches on the value of major-mode."
  ;; Doesn't need to do anything by default.
  t)

(cl-defgeneric ebdb-popup-window (major-mode)
  "Return a spec for how to pop up a window on an *EBDB* buffer.

This generic function dispatches on the current value of
major-mode.  The return value should be a three-element list
of (window horiz-p split), in which WINDOW is the window to
split, HORIZ-P is t if the window should be split horizontally,
else vertically, and SPLIT is either an integer, specifying
number of rows/columns, or a float specifying what percentage of
window real estate the pop-up should occupy.

Alternately, the return value can be nil, which means continue
using the current window.")

(cl-defmethod ebdb-popup-window (&context (major-mode ebdb-mode))
  "When popping up from an existing *EBDB* buffer, just reuse the window.

Ie, don't pop up at all."
  nil)

(cl-defmethod ebdb-popup-window ()
  "When popping up from a random window, use half the window."
  (let ((horiz-p (> (window-total-width) (window-total-height))))
    (list (get-buffer-window) horiz-p 0.5)))

;;;###autoload
(defun ebdb-mua-update-records (&optional header-class all)
  "Update all records associated with the message under point.

This command is meant for manually updating records when
`ebdb-mua-auto-update-p' is nil: it behaves as if that option
were set to 'query.  The rules of `ebdb-select-message' still
apply, however."
  (interactive)
  ;; Temporarily copy and paste from `ebdb-mua-display-records',
  ;; refactor later.
  (let ((ebdb-message-all-addresses (or all ebdb-message-all-addresses))
	(fmt ebdb-default-multiline-formatter)
        records)
    (ebdb-mua-prepare-article)
    (setq records (ebdb-update-records
		   (ebdb-get-address-components header-class)
		   'query t))
    (if records (ebdb-display-records records fmt nil nil (ebdb-popup-window)))
    records))

;;;###autoload
(defun ebdb-mua-display-records (&optional header-class all)
  "Display the EBDB record(s) for the addresses in this message.
This looks into the headers of a message according to HEADER-CLASS.
Then for the mail addresses found the corresponding EBDB records are displayed.
Records are not created or updated.

HEADER-CLASS is defined in `ebdb-message-headers'.  If it is nil,
use all classes in `ebdb-message-headers'.  If ALL is non-nil,
bind `ebdb-message-all-addresses' to ALL."
  (interactive)
  (let ((ebdb-message-all-addresses (or all ebdb-message-all-addresses))
	(fmt ebdb-default-multiline-formatter)
        records)
    (ebdb-mua-prepare-article)
    (setq records (ebdb-update-records
		   (ebdb-get-address-components header-class)
		   'existing t))
    (if records (ebdb-display-records records fmt nil nil (ebdb-popup-window)))
    records))

;; The following commands are some frontends for `ebdb-mua-display-records',
;; which is always doing the real work.  In your init file, you can further
;; modify or adapt these simple commands to your liking.

;;;###autoload
(defun ebdb-mua-display-sender ()
  "Display the EBDB record(s) for the sender of this message."
  (interactive)
  (ebdb-mua-display-records 'sender))

;;;###autoload
(defun ebdb-mua-display-recipients ()
  "Display the EBDB record(s) for the recipients of this message."
  (interactive)
  (ebdb-mua-display-records 'recipients))

;;;###autoload
(defun ebdb-mua-display-all-records ()
  "Display the EBDB record(s) for all addresses in this message."
  (interactive)
  (ebdb-mua-display-records nil t))

;;;###autoload
(defun ebdb-mua-display-all-recipients ()
  "Display EBDB records for all recipients of this message."
  (interactive)
  (ebdb-mua-display-records 'recipients t))

;; The commands `ebdb-annotate-record' and `ebdb-mua-edit-field'
;; have kind of similar goals, yet they use rather different strategies.
;; `ebdb-annotate-record' is less obtrusive.  It does not display
;; the records it operates on, nor does it display the content
;; of the field before or after adding or replacing the annotation.
;; Hence the user needs to know what she is doing.
;; `ebdb-mua-edit-field' is more explicit:  It displays the records
;; as well as the current content of the field that gets edited.

;; In principle, this function can be used not only with MUAs.
(defun ebdb-annotate-record (record annotation &optional field _replace)
  "In RECORD add an ANNOTATION to field FIELD.
FIELD defaults to `ebdb-annotate-field'.
If REPLACE is non-nil, ANNOTATION replaces the content of FIELD.
If ANNOTATION is an empty string and REPLACE is non-nil, delete FIELD."
  (if (memq field '(name firstname lastname phone address))
      (error "Field `%s' illegal" field))
  (setq annotation (ebdb-string-trim annotation))
  (cond ((memq field '(affix organization mail aka))
         (setq annotation (list annotation)))
        ((not field) (setq field ebdb-annotate-field)))
  (ebdb-record-change-field record field annotation))

;; FIXME: For interactive calls of the following commands, the arg UPDATE-P
;; should have the same meaning as for `ebdb-mua-display-records',
;; that is, it should use `ebdb-mua-update-interactive-p'.
;; But here the prefix arg is already used in a different way.
;; We could possibly solve this problem if all `ebdb-mua-*' commands
;; used another prefix arg that is consistently used only for
;; `ebdb-mua-update-interactive-p'.
;; Yet this prefix arg must be defined within the key space of the MUA(s).
;; This results in lots of conflicts...
;;
;; Current workaround:
;; These commands use merely the car of `ebdb-mua-update-interactive-p'.
;; If one day someone proposes a smart solution to this problem (suggestions
;; welcome!), this solution will hopefully include the current workaround
;; as a subset of all its features.

(defun ebdb-mua-annotate-field-interactive ()
  "Interactive specification for `ebdb-mua-annotate-sender' and friends."
  (let ((field (if (eq 'all-fields ebdb-annotate-field)
                   (intern (completing-read
                            "Field: "
                            (mapcar 'symbol-name
                                    '(affix organization mail aka))))
                 ebdb-annotate-field)))
    (list (read-string (format "Annotate `%s': " field))
          field current-prefix-arg)))

;;;###autoload
(defun ebdb-mua-annotate-sender (annotation &optional field replace)
  "Add ANNOTATION to field FIELD of the EBDB record(s) of message sender(s).
FIELD defaults to `ebdb-annotate-field'.
If REPLACE is non-nil, ANNOTATION replaces the content of FIELD."
  (interactive (ebdb-mua-annotate-field-interactive))
  (ebdb-mua-prepare-article)
  (dolist (record (ebdb-update-records
		   (ebdb-get-address-components 'sender)
		   'existing))
    (ebdb-annotate-record record annotation field replace)))

;;;###autoload
(defun ebdb-mua-annotate-recipients (annotation &optional field replace)
  "Add ANNOTATION to field FIELD of the EBDB records of message recipients.
FIELD defaults to `ebdb-annotate-field'.
If REPLACE is non-nil, ANNOTATION replaces the content of FIELD."
  (interactive (ebdb-mua-annotate-field-interactive))
  (ebdb-mua-prepare-article)
  (dolist (record (ebdb-update-records
		   (ebdb-get-address-components 'recipients)
		   'existing))
    (ebdb-annotate-record record annotation field replace)))

;;;###autoload
(defun ebdb-mua-edit-field (&optional field header-class)
  "Edit FIELD of the EBDB record(s) of message sender(s) or recipients.
FIELD defaults to value of variable `ebdb-mua-edit-field'.
HEADER-CLASS is defined in `ebdb-message-headers'.  If it is nil,
use all classes in `ebdb-message-headers'."
  (interactive)
  (cond ((memq field '(firstname lastname address phone))
         (error "Field `%s' not editable this way" field))
        ((not field)
         (setq field ebdb-mua-edit-field)))
  (ebdb-mua-prepare-article)
  (let ((records (ebdb-update-records
		  (ebdb-get-address-components header-class)
		  'existing))
	field-instance slot)
    (when records
      (ebdb-display-records records nil nil nil (ebdb-popup-window))
      (ebdb-with-record-edits (record records)
	;; All this is very bad, we need to rework `ebdb-edit-foo' so
	;; it can be used here.
	(setq field-instance (ebdb-record-field record field))
	(if field-instance
	    (ebdb-record-change-field record field-instance)
	  (setq field-instance (ebdb-read field)
		slot (car (ebdb-record-field-slot-query
			   (eieio-object-class record) `(nil . ,field))))
	  (ebdb-record-insert-field record slot field-instance))))))

;;;###autoload
(defun ebdb-mua-edit-field-sender (&optional field)
  "Edit FIELD of record corresponding to sender of this message.
FIELD defaults to value of variable `ebdb-mua-edit-field'."
  (interactive)
  (ebdb-mua-edit-field field 'sender))

;;;###autoload
(defun ebdb-mua-edit-field-recipients (&optional field)
  "Edit FIELD of record corresponding to recipient of this message."
  (interactive)
  (ebdb-mua-edit-field field 'recipients))

;; Functions for noninteractive use in MUA hooks

;;;###autoload
(defun ebdb-mua-auto-update (&optional header-class update-p)
  "Update EBDB automatically based on incoming and outgoing messages.
This looks into the headers of a message according to HEADER-CLASS.
Then for the mail addresses found the corresponding EBDB records are updated.
UPDATE-P determines whether only existing EBDB records are taken
or whether also new records are created for these mail addresses.
Return matching records.

HEADER-CLASS is defined in `ebdb-message-headers'.  If it is nil,
use all classes in `ebdb-message-headers'.  UPDATE-P may take the
same values as `ebdb-mua-auto-update-p'.  If UPDATE-P is nil,
use `ebdb-mua-auto-update-p' (which see).

If `ebdb-mua-pop-up' is non-nil, EBDB pops up the *EBDB* buffer
along with the MUA window(s), displaying the matching records
using `ebdb-pop-up-layout'.
If this is nil, EBDB is updated silently.

This function is intended for noninteractive use via appropriate MUA hooks.
Call `ebdb-mua-auto-update-init' in your init file to put this function
into the respective MUA hooks.
See `ebdb-mua-display-records' and friends for interactive commands."
  (let* ((ebdb-silent-internal t)
	 records)
    (setq records (ebdb-update-records
		   (ebdb-get-address-components header-class)
		   (or update-p
		       ebdb-mua-auto-update-p)))
    (if ebdb-mua-pop-up
	(if records
	    (ebdb-display-records records ebdb-default-multiline-formatter
				  nil nil (ebdb-popup-window))
	  ;; If there are no records, empty the EBDB window.
	  (ebdb-undisplay-records)))
    records))

;;;###autoload
(defun ebdb-auto-notes (record)
  "Automatically annotate RECORD based on the headers of the current message.
See the variables `ebdb-auto-notes-rules', `ebdb-auto-notes-ignore-messages'
and `ebdb-auto-notes-ignore-headers'.
For use as an element of `ebdb-notice-record-hook'."
  ;; This code re-evaluates the annotations each time a message is viewed.
  ;; It would be faster if we could somehow store (permanently?) that we
  ;; have already annotated a message.
  (let ((case-fold-search t))
    (unless ;; check the ignore-messages pattern
	(let ((ignore-messages ebdb-auto-notes-ignore-messages)
	      ignore rule)
	  (while (and (not ignore) (setq rule (pop ignore-messages)))
	    (if (cond ((functionp rule)
		       ;; RULE may use `ebdb-update-records-address'
		       (funcall rule record))
		      ((symbolp rule)
		       (eq rule (nth 4 ebdb-update-records-address)))
		      ((eq 1 (safe-length rule))
		       (ebdb-message-header-re (car rule) (cdr rule)))
		      ((eq 2 (safe-length rule))
		       (and (eq (car rule) (nth 4 ebdb-update-records-address))
			    (ebdb-message-header-re (nth 1 rule) (nth 2 rule)))))
		(setq ignore t)))
	  ignore)

      ;; For speed-up expanded rules are stored in `ebdb-auto-notes-rules-expanded'.
      (when (and ebdb-auto-notes-rules
                 (not ebdb-auto-notes-rules-expanded))
        (let (expanded mua from-to header)
          (dolist (rule ebdb-auto-notes-rules)
            ;; Which MUA do we want?
            (if (or (stringp (car rule))
                    (stringp (nth 1 rule)))
                (setq mua t)
              (setq mua (if (symbolp (car rule)) (listp (car rule)) (car rule))
                    rule (cdr rule)))
            ;; Which FROM-TO headers do we want?
            (if (stringp (car rule))
                (setq from-to t)
              (setq from-to (car rule)
                    rule (cdr rule)))
            (setq header (car rule))
            (let (string field replace elt-e)
              (dolist (elt (cdr rule))
                (if (consp (setq string (cdr elt)))
                    (setq field (car string) ; (REGEXP FIELD-NAME STRING REPLACE)
                          replace (nth 2 string) ; perhaps nil
                          string (nth 1 string))
                  ;; else it's simple (REGEXP . STRING)
                  (setq field ebdb-default-xfield
                        replace nil))
                (push (list (car elt) field string replace) elt-e))
              (push (append (list mua from-to header) (nreverse elt-e)) expanded)))
          (setq ebdb-auto-notes-rules-expanded (nreverse expanded))))

      (dolist (rule ebdb-auto-notes-rules-expanded)
        (let ((mua (car rule)) (from-to (nth 1 rule)) (header (nth 2 rule))
              hd-val string annotation)
          (when (and (or (eq mua t)
                         (memq (nth 4 ebdb-update-records-address) mua))
                     (or (eq from-to t)
                         (member-ignore-case
                          (nth 2 ebdb-update-records-address) from-to)
                         (memq (nth 3 ebdb-update-records-address) from-to))
                     (setq hd-val (ebdb-message-header header)))
            (dolist (elt (nthcdr 3 rule))
              (when (and (string-match (car elt) hd-val)
                         (let ((ignore (cdr (assoc-string
                                             header
                                             ebdb-auto-notes-ignore-headers t))))
                           (not (and ignore (string-match ignore hd-val)))))
                (setq string (nth 2 elt)
                      annotation
                      (cond ((integerp string)
                             (match-string string hd-val))
                            ((stringp string)
                             (replace-match string nil nil hd-val))
                            ((functionp string)
                             (funcall string hd-val))
                            (t (error "Illegal value: %s" string))))
                (ebdb-annotate-record record annotation
                                      (nth 1 elt) (nth 3 elt))))))))))

;;; Mark EBDB records in the MUA summary buffer

(defun ebdb-mua-summary-unify (address)
  "Unify mail ADDRESS displayed for a message in the MUA Summary buffer.
Typically ADDRESS refers to the value of the From header of a message.
If ADDRESS matches a record in EBDB display a unified name instead of ADDRESS
in the MUA Summary buffer.

Unification uses `ebdb-mua-summary-unification-list' (see there).
The first match in this list becomes the text string displayed
for a message in the MUA Summary buffer instead of ADDRESS.
If variable `ebdb-mua-summary-mark' is non-nil use it to precede known addresses.
Return the unified mail address.

Currently this works with Gnus and VM.  It requires the EBDB insinuation
of these MUAs.  Also, the MUA Summary format string must use
`ebdb-mua-summary-unify-format-letter' (see there)."
  ;; ADDRESS is analyzed as in `ebdb-get-address-components'.
  (let* ((data (ebdb-extract-address-components address))
         (name (car data))
         (mail (cadr data))
         (record (car (ebdb-message-search name mail)))
         (u-list ebdb-mua-summary-unification-list)
         elt val)
    (while (setq elt (pop u-list))
      (setq val (cond ((eq elt 'message-name) name)
                      ((eq elt 'message-mail) mail)
                      ((eq elt 'message-address) address)
                      (record (let ((result (ebdb-record-field record elt)))
                                (if (atom result) (ebdb-string result)
                                  (ebdb-string (car result)))))))
      (if val (setq u-list nil)))
    (format "%s%s"
            (cond ((not ebdb-mua-summary-mark) "")
                  ((not record) " ")
                  (t
		   (or (car-safe (ebdb-record-field record 'ebdb-field-summary-mark))
		       (ebdb-mua-make-summary-mark record)
		       ebdb-mua-summary-mark)))
            (or val name mail address "**UNKNOWN**"))))

(defun ebdb-mua-summary-mark (address)
  "In the MUA Summary buffer mark messages matching a EBDB record.
ADDRESS typically refers to the value of the From header of a message.
If ADDRESS matches a record in EBDB return a mark, \" \" otherwise.
The mark itself is the value of the xfield `ebdb-mua-summary-mark-field'
if this xfield is in the poster's record, and `ebdb-mua-summary-mark' otherwise."
  (if (not ebdb-mua-summary-mark)
      "" ; for consistency
    ;; ADDRESS is analyzed as in `ebdb-get-address-components'.
    (let* ((data (ebdb-extract-address-components address))
           (record (car (ebdb-message-search (car data) (cadr data)))))
      (if record
          (or (car-safe (ebdb-record-field record 'ebdb-field-summary-mark))
	      (ebdb-mua-make-summary-mark record)
              ebdb-mua-summary-mark)
        " "))))

(provide 'ebdb-mua)
;;; ebdb-mua.el ends here
