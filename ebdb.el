;;; ebdb.el --- Contact management package           -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Free Software Foundation, Inc.

;; Version: 0.1
;; Package-Requires: ((cl-lib "0.5") (seq "2.15"))

;; Maintainer: Eric Abrahamsen <eric@ericabrahamsen.net>

;; Author: Eric Abrahamsen <eric@ericabrahamsen.net>
;; Keywords: convenience mail

;; URL: https://github.com/girzel/ebdb

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

;; This package began as a port of the Insidious Big Brother Database
;; (by Jamie Zawinski and Roland Winkler) using EIEIO, Emacs' newish
;; object-orientation library.  Eventually it developed to the point
;; where a separate package seemed to make most sense.  But the basic
;; behavior of the package, and the look of the *EBDB* buffer, are
;; still very much indebted to the original library.  Some of the
;; original files have been directly copied over and renamed -- where
;; applicable, the names of the authors of the original libraries have
;; been noted.

;; This file contains the basic data structures and behavior for EBDB,
;; including the class definitions for databases, records, and fields.

;;; Code:

(require 'timezone)
(require 'cl-lib)
(require 'seq)
(require 'pcase)
(require 'eieio)
(require 'eieio-base)
(require 'eieio-opt)

(eval-when-compile              ; pacify the compiler.
  (autoload 'widget-group-match "wid-edit")
  (autoload 'ebdb-migrate "ebdb-migrate")
  (autoload 'ebdb-do-records "ebdb-com")
  (autoload 'ebdb-append-display-p "ebdb-com")
  (autoload 'ebdb-toggle-records-layout "ebdb-com")
  (autoload 'ebdb-dwim-mail "ebdb-com")
  (autoload 'ebdb-spec-prefix "ebdb-com")
  (autoload 'ebdb-completing-read-records "ebdb-com")
  (autoload 'mail-position-on-field "sendmail")
  (autoload 'vm-select-folder-buffer "vm-folder")
  (autoload 'eieio-customize-object "eieio-custom")
  ;; cannot use autoload for variables...
  (defvar message-mode-map) ;; message.el
  (defvar mail-mode-map) ;; sendmail.el
  (defvar gnus-article-buffer)) ;; gnus-art.el

;; These are the most important internal variables, holding EBDB's
;; data structures.

(defvar ebdb-db-list nil
  "The list of currently-loaded EBDB databases.")

(defvar ebdb-record-tracker nil
  "A list of all the loaded records")

(defvar ebdb-seen-uuids nil
  "A list of all previously-loaded UUIDs.")

(defvar ebdb-hashtable (make-hash-table :test 'equal)
  "Hash table for EBDB records.
Hashes the fields first-last-name, last-first-name, organization, aka,
and mail.")

(defvar ebdb-org-hashtable (make-hash-table :size 500 :test 'equal)
  "Hash table holding relationships between organizations and
  member records.

Keys are string UUIDs of organizations. Values are lists
of (record-uuid . role-field). Hashtable entries are created
and deleted by the `ebdb-init' and `ebdb-delete' methods of the
`ebdb-field-role' field class.")

;;; Internal variables
(eval-and-compile
  (defvar ebdb-debug t
    "Enable debugging if non-nil during compile time.
You really should not disable debugging.  But it will speed things up."))

(defconst ebdb-file-coding-system 'utf-8
  "Coding system used for reading and writing `ebdb-file'.")

(defvar ebdb-version "e"
  "EBDB version.")

(defvar ebdb-version-date "October 15, 2016"
  "Date this version of EBDB was released.")

(defvar ebdb-mail-aliases-need-rebuilt nil
  "Non-nil if mail aliases need to be rebuilt.")

(defvar ebdb-silent-internal nil
  "Bind this to t to quiet things down - do not set it.
See also `ebdb-silent'.")

;; Custom groups

(defgroup ebdb-eieio nil
  "Options for an EIEIO version of EBDB."
  :group 'ebdb)

(defcustom ebdb-sources "~/.emacs.d/ebdb"
  "User option specifying where EBDB contacts should be loaded
from.  It can be a single element, or a list of multiple
elements.  If an element is a string, it is treated as a
filename, and used to create an instance of `ebdb-db-file'.

Elements can also be instances of subclasses of `ebdb-db'.
Currently, the only subclass is `ebdb-db-file', though you can
create your own.  When EBDB is loaded, the `ebdb-db-load' method
will be called on each of class instances."
  :group 'ebdb-eieio
  :type 'string)

(defcustom ebdb-auto-merge-records nil
  "If t, EBDB will automatically merge multiple records with the
same UUID.

If you are using multiple databases, and intend to keep some
records in more than one database at once, you can set this to t
to have EBDB treat records with identical UUIDs as \"the same\"
record, and merge them automatically when the databases are
loaded.  If it is nil, you'll be prompted to do an interactive
merge.

Merging is currently \"dumb\", ie the record with the older
timestamp is essentially deleted and replaced by the newer.
Future merging strategies may be smarter."
  :group 'ebdb-eieio
  :type 'boolean)

(defcustom ebdb-default-record-class 'ebdb-record-person
  "The default class to use for new records."
  :group 'ebdb-eieio
  :type 'class)

(defcustom ebdb-default-name-class 'ebdb-field-name-complex
  "The default name class to use for person records.

Organization names are currently hard-coded to use
`ebdb-field-name-simple'."
  :group 'ebdb-eieio
  :type 'class)

(defcustom ebdb-default-mail-class 'ebdb-field-mail
  "The default class to use for mail fields."
  :group 'ebdb-eieio
  :type 'class)

(defcustom ebdb-default-phone-class 'ebdb-field-phone
  "The default class to use for phone fields."
  :group 'ebdb-eieio
  :type 'class)

(defcustom ebdb-default-address-class 'ebdb-field-address
  "The default class to use for address fields."
  :group 'ebdb-eieio
  :type 'class)

(defcustom ebdb-default-notes-class 'ebdb-field-notes
  "The default class to use for notes fields."
  :group 'ebdb-eieio
  :type 'class)

(defgroup ebdb nil
  "The Insidious Big Brother Database."
  :group 'news
  :group 'mail)

(defgroup ebdb-record-edit nil
  "Variables that affect the editing of EBDB records"
  :group 'ebdb)

(defgroup ebdb-sendmail nil
  "Variables that affect sending mail."
  :group 'ebdb)

(defgroup ebdb-utilities nil
  "Customizations for EBDB Utilities"
  :group 'ebdb)

(defgroup ebdb-utilities-dialing nil
  "EBDB Customizations for phone number dialing"
  :group 'ebdb)

(defgroup ebdb-utilities-print nil
  "Customizations for printing the EBDB."
  :group 'ebdb)
(put 'ebdb-utilities-print 'custom-loads '(ebdb-print))

(defgroup ebdb-utilities-anniv nil
  "Customizations for EBDB Anniversaries"
  :group 'ebdb-utilities)
(put 'ebdb-utilities-anniv 'custom-loads '(ebdb-anniv))

(defgroup ebdb-utilities-ispell nil
  "Customizations for EBDB ispell interface"
  :group 'ebdb-utilities)
(put 'ebdb-utilities-ispell 'custom-loads '(ebdb-ispell))

(defgroup ebdb-utilities-snarf nil
  "Customizations for EBDB snarf"
  :group 'ebdb-utilities)
(put 'ebdb-utilities-snarf 'custom-loads '(ebdb-snarf))

(defgroup ebdb-utilities-pgp nil
  "Customizations for EBDB pgp"
  :group 'ebdb-utilities)
(put 'ebdb-utilities-pgp 'custom-loads '(ebdb-pgp))

(defgroup ebdb-utilities-sc nil
  "Customizations for using Supercite with the EBDB."
  :group 'ebdb-utilities
  :prefix "ebdb-sc")
(put 'ebdb-utilities-sc 'custom-loads '(ebdb-sc))

;;; Customizable variables
(defcustom ebdb-image nil
  "The default method for displaying record images.

If a record is given a `ebdb-field-image' field, the value of
that field specifies how or where to find the image for the
record.  This option provides a default for that value.

If the field value is `name' or `fl-name', the first and last
name of the record are used as file name.  If it is `lf-name',
the last and first name of the record are used as file name.

If it is a string, that string is assumed to be a filename and
the file is searched in the directories in `ebdb-image-path'.
File name suffixes are appended according to
`ebdb-image-suffixes'.  See `locate-file'.

If it is nil, the method `ebdb-field-image-function' will be
called with two arguments: the image field and the record.  The
function should return either a filename, or actual image data."
  :group 'ebdb-record-edit
  :type '(choice (const :tag "Use built-in function" nil)
                 (const name)
		 (const fl-name)
		 (const lf-name)))

(defcustom ebdb-uuid-function "uuidgen"
  "Function used for creating a UUID for records.

If a string, assume a system executable.  If a symbol, assume an
elisp function for creating UUIDs.  For instance, `org-id-uuid'
is a good candidate."
  :group 'ebdb
  :type '(or string symbol))

(defcustom ebdb-record-self nil
  "The UUID of the record representing the user.

See the docstring of `ebdb-user-mail-address-re' for possible uses."
  :group 'ebdb
  :type 'string)

(defcustom ebdb-country-list nil
  "A list of country names known to EBDB.

This is a list of simple strings, which do not change EBDB's
behavior in any way.  You can also require the \"ebdb-i18n\"
library for more internationally-aware functionality, in which
case this variable will be ignored."
  :group 'ebdb
  :type '(list-of string))

(defcustom ebdb-auto-revert nil
  "If t revert unchanged database without querying.

If t and a database file has changed on disk, while the database
has not been modified inside Emacs, revert the database
automatically.  If nil or the database has been changed inside
Emacs, always query before reverting."  :group 'ebdb :type
'(choice (const :tag "Revert unchanged database without querying"
t)
                 (const :tag "Ask before reverting database" nil)))

(defcustom ebdb-before-load-hook nil
  "Hook run before loading databases."
  :group 'ebdb
  :type 'hook)

(defcustom ebdb-after-load-hook nil
  "Hook run after loading databases."
  :group 'ebdb
  :type 'hook)

(defvar ebdb-create-hook nil
  "*Hook run each time a new EBDB record is created.
Run with one argument, the new record.  This is called before the record is
added to the database, followed by a call of `ebdb-change-hook'.

If a record has been created by analyzing a mail message, hook functions
can use the variable `ebdb-update-records-address' to determine the header
and class of the mail address according to `ebdb-message-headers'.")

(defvar ebdb-change-hook nil
  "*Hook run each time a EBDB record is changed.
Run with one argument, the record.  This is called before the database
is modified.  If a new ebdb record is created, `ebdb-create-hook' is called
first, followed by a call of this hook.")

(defcustom ebdb-time-format "%Y-%m-%d %T %z"
  "The EBDB time stamp format.  Used for human-readable display
of timestamp values."
  :group 'ebdb
  :type 'string)

(defcustom ebdb-after-change-hook nil
  "Hook run each time a EBDB record is changed.
Run with one argument, the record.  This is called after the database
is modified.  So if you want to modify a record when it is created or changed,
use instead `ebdb-create-hook' and / or `ebdb-change-hook'."
  :group 'ebdb
  :type 'hook)

;; These two hooks could actually be done away with, and replaced by
;; method overloads on `ebdb-db-load'.  But users will probably be
;; more familiar with hooks than with method overloading.
(defcustom ebdb-before-load-db-hook nil
  "Hook run before each database is loaded.
Run with one argument, the database being loaded."
  :group 'ebdb
  :type 'hook)

(defcustom ebdb-after-load-db-hook nil
  "Hook run after each database is loaded.
Run with one argument, the database being loaded."
  :group 'ebdb
  :type 'hook)

(defcustom ebdb-initialize-hook nil
  "Normal hook run after the EBDB initialization function `ebdb-initialize'."
  :group 'ebdb
  :type 'hook)

(defcustom ebdb-mode-hook nil
  "Normal hook run when the *EBDB* buffer is created."
  :group 'ebdb
  :type 'hook)

(defcustom ebdb-silent nil
  "If t, EBDB suppresses all its informational messages and queries.
Be very very certain you want to set this to t, because it will suppress
queries to alter record names, assign names to addresses, etc.
Lisp Hackers: See also `ebdb-silent-internal'."
  :group 'ebdb
  :type '(choice (const :tag "Run silently" t)
                 (const :tag "Disable silent running" nil)))

(defcustom ebdb-info-file nil
  "Location of the ebdb info file, if it's not in the standard place."
  :group 'ebdb
  :type '(choice (const :tag "Standard location" nil)
                 (file :tag "Nonstandard location")))

(defvar ebdb-update-unchanged-records nil
  "If non-nil update unchanged records in the database.
Normally calls of `ebdb-change-hook' and updating of a record are suppressed,
if an editing command did not really change the record.  Bind this to t
if you want to call `ebdb-change-hook' and update the record unconditionally.")

(defvar ebdb-street-list nil
  "List of streets known to EBDB.")

(defvar ebdb-locality-list nil
  "List of localities (towns or cities) known to EBDB.")

(defvar ebdb-region-list nil
  "List of regions (states or provinces) known to EBDB.")

(defvar ebdb-postcode-list nil
  "List of post codes known to EBDB.")

;;; Define some of our own errors. A few of these should never be
;;; shown to the user, they're for internal flow control.

;; Error parent
(define-error 'ebdb-error "EBDB error")

(define-error 'ebdb-duplicate-uuid "Duplicate EBDB UUID" 'ebdb-error)

(define-error 'ebdb-unsynced-db "EBDB DB unsynced" 'ebdb-error)

(define-error 'ebdb-disabled-db "EBDB DB disabled" 'ebdb-error)

(define-error 'ebdb-readonly-db "EBDB DB read-only" 'ebdb-error)

(define-error 'ebdb-unacceptable-field "EBDB record cannot accept field" 'ebdb-error)

(define-error 'ebdb-empty "Empty value" 'ebdb-error)

(define-error 'ebdb-unparseable "Unparseable value" 'ebdb-error)

;;; Utility functions and macros

;;;###autoload
(defsubst ebdb-records (&optional record-class child-p)
  "Return a list of all EBDB records; load databases if necessary.
This function also notices if databases are out of sync.

If RECORD-CLASS is given, only return records of this class or,
if CHILD-P is non-nil, one of its subclasses."
  (unless ebdb-db-list
    (ebdb-load))
  (if record-class
      (seq-filter
       (lambda (r)
	 (if child-p
	     (object-of-class-p r record-class)
	   (same-class-p r record-class)))
       ebdb-record-tracker)
   ebdb-record-tracker))

(defmacro ebdb-error-retry (&rest body)
  "Repeatedly execute BODY ignoring errors till no error occurs."
  `(catch '--ebdb-error-retry--
     (while t
       (condition-case --c--
           (throw '--ebdb-error-retry-- (progn ,@body))
         (ebdb-unparseable
	  (ding)
	  (message "Error: %s" (nth 1 --c--))
	  (sit-for 2))))))

(defmacro ebdb-with-exit (&rest body)
  `(condition-case nil
       ,@body
     ((quit ebdb-empty)
      nil)))

(defmacro ebdb-loop-with-exit (&rest body)
  "Repeat BODY, accumulating the results in a list, until the
user either hits C-g, or enters an empty field label."
  `(let (acc)
     (catch '--ebdb-loop-exit--
       (condition-case nil
	   (while t
	     (push ,@body acc))
	 ((quit ebdb-empty)
	  (throw '--ebdb-loop-exit-- acc))))))

(defmacro ebdb-debug (&rest body)
  "Excecute BODY just like `progn' with debugging capability.
Debugging is enabled if variable `ebdb-debug' is non-nil during compile.
You really should not disable debugging.  But it will speed things up."
  (declare (indent 0))
  (if ebdb-debug ; compile-time switch
      `(let ((debug-on-error t))
         ,@body)))

;;; Fields.

(defclass ebdb-field ()
  ((actions
    :type (list-of function)
    :allocation :class
    :initform nil
    :documentation
    "A list of actions which this field can perform."))
  :abstract t
  :documentation "Abstract class for EBDB fields.  Subclass this
to produce real field types.")

(cl-defmethod ebdb-init (_field-value &rest _args)
  "Catch-all `ebdb-init' method for fields.

This method may also get called on field values that aren't
actually `ebdb-field' instances -- for instance, plain strings.
In those cases, assume we don't need to do anything."
  t)

(cl-defmethod ebdb-field-readable-name ((field (subclass ebdb-field)))
  "Return a human-readable string label for this class.

Mostly used for allowing users to pick which field type they want
to add to a record."
  ;; Why is there no non-private access to this?  The `class-option'
  ;; function is mentioned in the EIEIO manual, but doesn't exist.
  (eieio--class-option (find-class field) :human-readable))

(cl-defmethod ebdb-field-readable-name ((field ebdb-field))
  (ebdb-field-readable-name (eieio-object-class field)))

(cl-defmethod ebdb-field-readable-name ((_field (eql string)))
  "Value")

;;; Errors

;; I haven't figured this out quite yet.  What I want to do is avoid
;; raising errors for *some* methods, with *some* classes; right now
;; all errors are suppressed.  It doesn't seem very easy to specialize
;; on methods and classes here: the GENERIC argument that's passed in
;; to the methods below is the full struct of the generic itself.
;; Presumably I'll have to look into that struct?  Or maybe I should
;; just write bottom-level do-nothing methods for the cases where I
;; don't want to raise an error.  I guess I'll do that for
;; `ebdb-delete' and `ebdb-init', for the base `ebdb-field' class.

;; (cl-defmethod cl-no-applicable-method (_generic &rest _args)
;;   "Don't raise errors for unimplemented methods."
;;   (message "All no-applicable-method errors are swallowed."))

;; (cl-defmethod cl-no-next-method (_generic _method &rest _args)
;;   "Don't raise errors for non-existent next methods."
;;   (message "All no-next-method errors are swallowed."))

;; There used to be a `destructor' method, but it's been marked
;; obsolete as of 25.2.  There may be a `delete-instance' method, but
;; then again there may not.  Handle it ourselves.

(cl-defmethod ebdb-delete ((field ebdb-field) &optional _record _unload)
  "User-level deletion routine for FIELD.

Override this to do any necessary cleanup work after FIELD is
removed."
  (delete-instance field))

(cl-defmethod ebdb-delete ((_field string) &optional _record _unload)
  t)

(cl-defmethod delete-instance ((_field ebdb-field) &rest _args)
  t)

(cl-defmethod ebdb-read ((class (subclass ebdb-field)) &optional slots _obj)
  "Complete the read/object creation process for a field of CLASS.

Earlier subclasses of `ebdb-field' will have read all the
necessary values into SLOTS; this base method is simply
responsible for creating the field object.

The OBJ argument is used when editing existing fields: OBJ is the
old field.  By now we've sucked all the useful information out of
it, and if this process is successful it will get deleted."
  (apply 'make-instance class slots))

;; Generics for fields.  Not all field classes will implement these
;; methods.  `ebdb-action' should raise an error (to be caught and
;; displayed at top level) when there is no applicable action method,
;; so we don't actually define a base method. `ebdb-notice' shouldn't
;; raise an error if it's not implemented, so we define a do-nothing
;; base method.

(cl-defmethod ebdb-action ((field ebdb-field) record &optional idx)
  "Do an \"action\" based on one of the functions listed in FIELD's action slot.

If IDX is provided, it is an index indicating which of the action
functions to call. Otherwise, call the car of the list."
  (let* ((actions (slot-value field 'actions))
	 (func (when actions
		 (if idx (or (nth idx actions) (last actions)) (car actions)))))
    (when func
      (funcall func record field))))

(cl-defmethod ebdb-notice ((_field ebdb-field) &optional _type _message-headers _record)
  "Ask FIELD of RECORD to react to RECORD being \"noticed\".

When the user receives an email from or cc'd to RECORD, that
record will call `ebdb-notice' on all its fields, and give them a
chance to react somehow.  TYPE is one of the symbols to, from, or
cc, indicating which message header the record was found in.
MESSAGE-HEADERS is a list of all the headers of the incoming
message."
  nil)

(cl-defmethod ebdb-field-search ((field ebdb-field) (regex string))
  (condition-case nil
      (string-match-p regex (ebdb-string field))
    (cl-no-applicable-method nil)))

;;; The UUID field.

;; This was originally just a string-value slot, but it was such a
;; pain in the neck differentiating between strings and fields that
;; I'm just making it a field.  Who knows, it might come in handy later.

(defclass ebdb-field-uuid (ebdb-field)
  ((uuid
    :type string
    :initarg :uuid
    :initform ""))
  :human-readable "uuid")

(cl-defmethod ebdb-string ((field ebdb-field-uuid))
  (slot-value field 'uuid))

;;; The name fields.  One abstract base class, and two instantiable
;;; subclasses.

;; TODO: Allow the user to choose whether the aka slot uses
;; `ebdb-field-name-simple' or `ebdb-field-name-complex'.  Or maybe on
;; a case-by-case basis?

(defclass ebdb-field-name (ebdb-field)
  nil
  :abstract t
  :documentation "Abstract base class for creating record
  names.")

(defclass ebdb-field-name-simple (ebdb-field-name)
  ((name
    :type string
    :initarg :name
    :custom string
    :initform ""))
  :documentation "A name class for \"simple\" names: ie plain
  strings."
  :human-readable "name")

(cl-defmethod ebdb-string ((name ebdb-field-name-simple))
  (slot-value name 'name))

(cl-defmethod ebdb-read ((class (subclass ebdb-field-name-simple))
			 &optional slots obj)
  (let ((name (ebdb-read-string "Name: " (when obj (slot-value obj name)))))
    (cl-call-next-method class (plist-put slots :name name) obj)))

(cl-defmethod ebdb-init ((name ebdb-field-name-simple) &optional record)
  (when record
    (ebdb-puthash (ebdb-string name) record))
  (cl-call-next-method))

(defclass ebdb-field-name-complex (ebdb-field-name)
  ((surname
    :initarg :surname
    :type (or null string)
    :custom (choice (const :tag "No surname" nil)
		    (string :tag "Surname"))
    :initform nil)
   (given-names
    :initarg :given-names
    :type (list-of string)
    :custom (repeat (string :tag "Name"))
    :initform nil)
   (prefix
    :initarg :prefix
    :type (or null string)
    :custom (choice (const :tag "No prefix" nil)
		    (string :tag "Prefix"))
    :initform nil)
   (suffix
    :initarg :suffix
    :type (or null string)
    :custom (choice (const :tag "No suffix" nil)
		    (string :tag "Suffic"))
    :initform nil)
   ;; What is an affix, actually?
   (affix
    :initarg :affix
    :type (or null string)
    :custom (choice (const :tag "No affix" nil)
		    (string :tag "Affix"))
    :initform nil))
  :documentation "A name class for \"complex\", ie structured,
  names."
  ;; This returns "aka" because the prompt is only used when _adding_
  ;; another name to an existing record.
  :human-readable "aka")

(cl-defmethod ebdb-name-last ((name ebdb-field-name-complex))
  "Return the surname of this name field."
  (slot-value name 'surname))

(cl-defmethod ebdb-name-given ((name ebdb-field-name-complex) &optional full)
  "Return the given names of this name field.

If FULL is t, return all the given names, otherwise just the
first one."
  (let ((given (slot-value name 'given-names)))
    (if full
	(mapconcat #'identity given " ")
      (car given))))

(cl-defmethod ebdb-name-lf ((name ebdb-field-name-complex) &optional full)
  (let ((given-string (ebdb-name-given name full))
	(prefix (slot-value name 'prefix)))
    (concat (ebdb-name-last name)
	    (when prefix prefix)
	    (when given-string (format ", %s" given-string)))))

(cl-defmethod ebdb-name-fl ((name ebdb-field-name-complex) &optional _full)
  (with-slots (prefix surname suffix) name
    (concat (ebdb-name-given name t)
	    " "
	    (when prefix
	      (format "%s " prefix))
	    (slot-value name 'surname)
	    (when suffix
	      (format ", %s" suffix)))))

(cl-defmethod ebdb-string ((name ebdb-field-name-complex))
  "Produce a canonical string for NAME."
  ;; This is the crux of things, really.  This is the method that
  ;; produces the name you'll see in the *EBDB* buffer, so this is the
  ;; bit that should be most customizable, and most flexible.  This
  ;; value also gets stored in the cache.
  (ebdb-name-fl name))

(cl-defmethod ebdb-init ((name ebdb-field-name-complex) &optional record)
  (when record
    (let ((lf (ebdb-name-lf name))
	  (fl (ebdb-name-fl name)))
	(ebdb-puthash lf record)
	(ebdb-puthash fl record)
	(object-add-to-list (ebdb-record-cache record) 'alt-names lf)
	(object-add-to-list (ebdb-record-cache record) 'alt-names fl)))
  (cl-call-next-method))

(cl-defmethod ebdb-delete ((name ebdb-field-name-complex) &optional record _unload)
  (when record
    (let ((lf (ebdb-name-lf name))
	  (fl (ebdb-name-fl name)))
      (ebdb-remhash lf record)
      (ebdb-remhash fl record)
      (object-remove-from-list (ebdb-record-cache record) 'alt-names lf)
      (object-remove-from-list (ebdb-record-cache record) 'alt-names fl)))
  (cl-call-next-method))

(cl-defmethod ebdb-read ((class (subclass ebdb-field-name-complex)) &optional slots obj)
  (if ebdb-read-name-articulate
      (let* ((surname-default (when obj (ebdb-name-last obj)))
	     (given-default (when obj (ebdb-name-given obj t)))
	     (surname (read-string "Surname: " surname-default))
	     (given-names (read-string "Given name(s): " given-default)))
	(setq slots (plist-put slots :surname surname))
	(setq slots (plist-put slots :given-names (split-string given-names)))
	(cl-call-next-method class slots obj))
    (ebdb-parse class (ebdb-read-string "Name: " (when obj (ebdb-string obj))))))

(cl-defmethod ebdb-field-search ((_field ebdb-field-name-complex) _regex)
  "Short-circuit the plain field search for names.

The record itself performs more complex searches on cached name
values, by default the search is not handed to the name field itself."
  nil)

(cl-defmethod ebdb-parse ((_class (subclass ebdb-field-name-complex)) string)
  (let ((bits (ebdb-divide-name string)))
    (make-instance 'ebdb-field-name-complex
     :given-names (when (car bits)
		    (list (car bits)))
     :surname (cdr bits))))

;;; The labeled abstract class.  Can be subclassed, or used as a mixin.

;; Probably there's no need to subclass from `ebdb-field'.  Without
;; this, it will be a pure mixin class.  Probably we can do away with
;; this inheritance without hurting anything.
(defclass ebdb-field-labeled (ebdb-field eieio-named)
  ;; Override the object-name slot from `eieio-named' so we can add a
  ;; custom declaration to it.
  ((object-name
    :initarg :object-name
    :custom (choice (const :tag "Empty" nil)
		    string)
    :initform nil)
   (label-list
    :initform nil
    :type (or null symbol)
    :allocation :class
    :documentation
    "This class-allocated slot points to a variable holding a
    list of known string labels for objects of the subclass."))
  :abstract t
  :documentation "A field with a string label.")

(cl-defmethod ebdb-read :around ((class (subclass ebdb-field-labeled)) &optional slots obj)
  "Prompt for a label for a new object of class CLASS, using OBJ
  as a default, and store the result in SLOTS.

All subclasses of `ebdb-field-labeled' should have a 'label-list
slot pointing to a var holding known labels for that class.  This
method checks that the label is known, and asks for confirmation
if it isn't.

This method also signals the 'ebdb-empty error if the user gives
an empty string as a label, which allows interruption of the read
process."
  ;; This is an :around method so the field label can be prompted for
  ;; before the value.
  (let* ((labels (symbol-value (oref-default class label-list)))
	 (human-readable (ebdb-field-readable-name class))
	 (label (plist-get slots :object-name)))
    (unless label
      (setq label (ebdb-read-string
		   (if (stringp human-readable)
		       (format "%s label: " (capitalize human-readable))
		     "Label: ")
		   (and obj (slot-value obj 'object-name))
		   labels nil)
	    slots (plist-put slots :object-name label)))
    (if (or (member label labels)
	    (yes-or-no-p (format "%s is not a known label, define it? " label)))
	(cl-call-next-method class slots obj)
      (signal 'ebdb-empty (list class)))))

(cl-defmethod ebdb-init ((field ebdb-field-labeled) &optional _record)
  (let ((label-var (slot-value field 'label-list)))
    (ebdb-add-to-list label-var (slot-value field 'object-name))
    (cl-call-next-method)))

;;; The obfuscated field type.  This is a little goofy, but might come
;;; in handy.

(defclass ebdb-field-obfuscated (ebdb-field)
  nil
  :abstract t
  :documentation
  "A field class mixin that prevents the contents from being
  displayed in the *EBDB* buffer.  Use for mildly sensitive
  information.")

;;; User-defined fields.  There are two kinds.  The first is
;;; `ebdb-field-user', which provides no information about labels or
;;; slots, but simply gives us the right to live in the "fields" slot
;;; of records.  It must be subclassed to be useful.

;;; The second is the `ebdb-field-user-simple', which subclasses
;;; `ebdb-field-user' and `ebdb-field-labeled'.  This class should
;;; *not* be subclassed; it's the class that collects all the basic
;;; label-plus-value fields that users might want to add.  Instances
;;; have no particular behavior, they're just key-value pairs.

(defclass ebdb-field-user (ebdb-field)
  nil
  :abstract t
  :documentation
  "Fields that should be user-editable, but need more complicated
  slot structures than the simple \"value\" provided by
  `ebdb-field-user-simple', can subclass this class.  Any field
  class that subclasses this will be offered as a choice to the
  user when inserting new fields.")

(defvar ebdb-user-label-list nil
  "List of existing labels of user fields.")

(defclass ebdb-field-user-simple (ebdb-field-labeled ebdb-field-user)
  ((label-list :initform ebdb-user-label-list)
   (value
    :initarg :value
    :type (or atom list)
    :initform ""
    :custom string
    :documentation "The value of this user-defined field."))
  :human-readable "user field")

(cl-defmethod ebdb-string ((field ebdb-field-user-simple))
  (let ((val (slot-value field 'value)))
    (if (stringp val)
	val
      (ebdb-concat (intern-soft (slot-value field 'object-name)) val))))

;; TODO: Maybe replicate the ability to insert a lisp sexp directly,
;; in interactive mode?
(cl-defmethod ebdb-read ((class (subclass ebdb-field-user-simple)) &optional slots obj)
  (unless (plist-get slots :value)
    (let ((default (when obj (ebdb-string obj))))
      (setq slots (plist-put slots :value (ebdb-read-string "Value: " default)))))
  (cl-call-next-method class slots obj))

;;; Role fields.

(defvar ebdb-role-label-list nil)

(defclass ebdb-field-role (ebdb-field-labeled ebdb-field)
  ((label-list :initform ebdb-role-label-list)
   (record-uuid
    :initarg :record-uuid
    :type (or null string)
    :initform nil)
   (org-uuid
    :initarg :org-uuid
    :type (or null string)
    :initform nil)
   (mail
    :initarg :mail
    :type (or null ebdb-field-mail)
    :initform nil)
   (fields
    :initarg :fields 
    :type (list-of ebdb-field)
    :initform nil)
   (defunct
     :initarg :defunct
     :type boolean
     :custom boolean
     :initform nil
     :documentation "If t, this role is considered defunct (ie
     the person left their job, etc).  Fields in the \"fields\"
     slot will generally be ignored by the rest of EBDB."))
  :documentation "This class represents a relationship between
  the record which owns this field, and the
  `ebdb-record-organization' pointed to by the \"organization\"
  slot.  The \"mail\" slot holds the record's organizational
  email address.  The \"fields\" slot holds whatever extra fields
  might be relevant to the role."
  :human-readable "role")

(cl-defmethod ebdb-init ((role ebdb-field-role) &optional record)
  (when record
    (let* ((org-uuid (slot-value role 'org-uuid))
	   (org (ebdb-gethash org-uuid 'uuid))
	   (org-string (if org
			   (ebdb-string (slot-value org 'name))
			 "record not loaded"))
	   ;; TODO: Guard against org-entry not being found.
	   (org-entry (gethash org-uuid ebdb-org-hashtable))
	   (record-uuid (ebdb-record-uuid record))
	   (role-mail (slot-value role 'mail))
	   new-org-entry exists-p)
      ;; TODO: We shouldn't really be doing this here, because setting
      ;; the slot value would mean the record/database should be set
      ;; to dirty, which it isn't necessarily, at this point.
      ;; Instead, `ebdb-read' should accept the record as an argument,
      ;; and this should be done in that method instead.
      (unless (slot-value role 'record-uuid)
	(setf (slot-value role 'record-uuid) record-uuid))
      (object-add-to-list (ebdb-record-cache record) 'organizations org-string)
      ;; Init the role mail against the record.
      (when (and role-mail (slot-value role-mail 'mail))
	(ebdb-init role-mail record))
      ;; Make sure this role is in the `ebdb-org-hashtable'.
      (unless (and org-entry
		   (dolist (pair org-entry exists-p)
		     (if (and (string= record-uuid (car pair))
			      (string= (slot-value (cdr pair) 'object-name)
				       (slot-value role 'object-name)))
			 (setq exists-p t)
		       (push pair new-org-entry))))
	;; TODO: It's no longer necessary to record the record-uuid
	;; along with the role, as the uuid is recorded in a slot on
	;; the role.
	(push (cons record-uuid role) new-org-entry))
      (puthash org-uuid new-org-entry ebdb-org-hashtable)))
  (when (slot-value role 'mail)
    (ebdb-init (slot-value role 'mail) record))
  (cl-call-next-method))

(cl-defmethod ebdb-delete ((role ebdb-field-role) &optional record unload)
  (when record
    (let* ((org-uuid (slot-value role 'org-uuid))
	   (org-string
	    (slot-value
	     (ebdb-gethash org-uuid 'uuid)
	     'name))
	   (org-entry (gethash org-uuid ebdb-org-hashtable))
	   (record-uuid (ebdb-record-uuid record))
	   record-entry new-org-entry)
      (while (setq record-entry (pop org-entry))
	;; The assumption being made here is that
	;; record+organization+role-label combinations must be unique.
	;; A record can have multiple roles at an organization, but
	;; those roles must have different labels.
	(unless (and (string= (car record-entry) record-uuid)
		     (string= (slot-value role 'object-name)
			      (slot-value (cdr record-entry) 'object-name)))
	  (push record-entry new-org-entry)))
      (puthash org-uuid new-org-entry ebdb-org-hashtable)      
      (when (null (assoc-string record-uuid org-entry))
	;; RECORD no long has any roles at ORG.
	(object-remove-from-list (ebdb-record-cache record) 'organizations org-string))))
  (when (slot-value role 'mail)
    (ebdb-delete (slot-value role 'mail) record unload))
  (cl-call-next-method))

(cl-defmethod ebdb-read ((role (subclass ebdb-field-role)) &optional slots obj)
  (let ((org-id (if obj (slot-value obj 'org-uuid)
		  (ebdb-record-uuid (ebdb-prompt-for-record nil 'ebdb-record-organization))))
	(mail (ebdb-with-exit
	       (ebdb-read ebdb-default-mail-class nil
			  (when obj (slot-value obj 'mail))))))
    (when mail
      (setq slots (plist-put slots :mail mail)))
    (setq slots (plist-put slots :org-uuid org-id))
    (cl-call-next-method role slots obj)))

(cl-defmethod ebdb-string ((role ebdb-field-role))
  "Display a string for this ROLE."
  ;; This is used in person records headers, so it just shows the
  ;; organization name. Perhaps this could have a multi-line option
  ;; later.
  (let ((org (ebdb-gethash (slot-value role 'org-uuid) 'uuid)))
    (if org
	(ebdb-string org)
      "record not loaded")))

;;; Mail fields.

;; Instead of one field holding many mail addresses, this is one field
;; per address.

(defclass ebdb-field-mail (ebdb-field)
  ((aka
    :initarg :aka
    :type (or null string)
    :custom (choice (const :tag "None" nil)
		    (string :tag "Mail AKA"))
    :initform nil
    :documentation "Alternate contact name for this address.")
   (mail
    :initarg :mail
    :type string
    :custom string
    :initform ""
    :documentation "Email address")
   (priority
    :initarg :priority
    :type (or null symbol)
    :initform normal
    :custom (choice (const :tag "Normal priority" normal)
		    (const :tag "Primary address" primary)
		    (const :tag "Defunct address" defunct))
    :documentation
    "The priority of this email address. The symbol 'normal means
    normal priority, 'defunct means the address will not be
    offered for completion, and 'primary means this address will
    be used as the default.  Only one of a record's addresses can
    be set to 'primary.")
   (actions :initform '(ebdb-mail)))
  :documentation "A field representing a single email address.
  The optional \"object-name\" slot can serve as a mail aka."
  :human-readable "mail")

(cl-defmethod ebdb-init ((mail ebdb-field-mail) &optional record)
  (with-slots (aka mail) mail
    (ebdb-puthash mail record)
    (object-add-to-list (ebdb-record-cache record) 'mail-canon mail)
    (when aka
      (ebdb-puthash aka record)
      (object-add-to-list (ebdb-record-cache record) 'mail-aka aka))))

(cl-defmethod ebdb-delete ((mail ebdb-field-mail) &optional record _unload)
  (with-slots (aka mail) mail
    (when record
      (when aka
	(ebdb-remhash aka record)
	(object-remove-from-list (ebdb-record-cache record) 'mail-aka aka))
      (ebdb-remhash mail record)
      (object-remove-from-list (ebdb-record-cache record) 'mail-canon mail)))
  (cl-call-next-method))

(cl-defmethod ebdb-string ((mail ebdb-field-mail))
  (with-slots (aka mail) mail
    (if aka
	(concat aka " <" mail ">")
      mail)))

(cl-defmethod ebdb-read ((class (subclass ebdb-field-mail)) &optional slots obj)
  (let* ((default (when obj (ebdb-string obj)))
	 (input (ebdb-read-string "Mail address: " default))
	 (bits (ebdb-decompose-ebdb-address input))
	 (mail (nth 1 bits)))
    ;; (unless (or ebdb-allow-duplicates
    ;; 		(and obj
    ;; 		     (equal mail (slot-value obj 'mail))))
    ;;   (when (ebdb-gethash mail '(mail))
    ;; 	(error "Address %s belongs to another record" mail)))
    (when (car bits)
      (setq slots (plist-put slots :aka (car bits))))
    (setq slots (plist-put slots :mail mail))
    (when obj
      (setq slots (plist-put slots :priority (slot-value obj 'priority))))
    (cl-call-next-method class slots obj)))

(defun ebdb-sort-mails (mails)
  "Sort MAILS (being instances of `ebdb-field-mail') by their
  priority slot.  Primary sorts before normal sorts before
  defunct."
  (sort
   mails
   (lambda (l r)
     (let ((l-p (slot-value l 'priority))
	   (r-p (slot-value r 'priority)))
       (or (and (eq l-p 'primary)
		(memq r-p '(normal defunct)))
	   (and (eq l-p 'normal)
		(eq r-p 'defunct)))))))

;;; Address fields

(defclass ebdb-field-address (ebdb-field-labeled ebdb-field)
  ((label-list :initform ebdb-address-label-list)
   (streets
    :initarg :streets
    :type (list-of string)
    :initform nil
    :custom (repeat string)
    :accessor ebdb-address-streets
    :documentation "A list of strings representing the street address(es).")
   (locality
    :initarg :locality
    :type string
    :initform ""
    :custom string
    :accessor ebdb-address-locality
    :documentation "City, town, village, etc.")
   (region
    :initarg :region
    :type string
    :initform ""
    :custom string
    :accessor ebdb-address-region
    :documentation "State, province, region, etc.")
   (postcode
    :initarg :postcode
    :type string
    :initform ""
    :custom string
    :accessor ebdb-address-postcode)
   (country
    :initarg :country
    :type (or string symbol)
    :initform ""
    :custom (choice (symbol)
		    (string))
    :accessor ebdb-address-country
    :documentation "Country, represented either by a symbol (see ebdb-i18n.el) or a string."))
  :documentation "A field representing an address."
  :human-readable "address")

(cl-defmethod ebdb-init ((address ebdb-field-address) &optional _record)
  (with-slots (object-name streets locality region postcode country) address
    (dolist (s streets)
      (ebdb-add-to-list 'ebdb-street-list s))
    (ebdb-add-to-list 'ebdb-locality-list locality)
    (when (stringp country)
     (ebdb-add-to-list 'ebdb-country-list country))
    (ebdb-add-to-list 'ebdb-region-list region)
    (ebdb-add-to-list 'ebdb-postcode-list postcode)))

(cl-defmethod ebdb-read ((class (subclass ebdb-field-address)) &optional slots obj)
  (let ((streets
	 (if (plist-member slots :streets)
	     (plist-get slots :streets)
	   (ebdb-edit-address-street (when obj (ebdb-address-streets obj)))))
	(locality
	 (if (plist-member slots :locality)
	     (plist-get slots :locality)
	   (ebdb-read-string "Town/City: "
			     (when obj (ebdb-address-locality obj)) ebdb-locality-list)))
	(region
	 (if (plist-member slots :region)
	     (plist-get slots :state)
	   (ebdb-read-string "State/Province: "
			     (when obj (ebdb-address-region obj)) ebdb-locality-list)))
	(postcode
	 (if (plist-member slots :postcode)
	     (plist-get slots :postcode)
	   (ebdb-error-retry
	    (ebdb-parse-postcode
	     (ebdb-read-string "Postcode: "
			       (when obj (ebdb-address-postcode obj))
			       ebdb-postcode-list)))))
	(country
	 (if (plist-member slots :country)
	     (plist-get slots :country)
	   (ebdb-read-string "Country: "
			     (if obj (slot-value obj 'country)
			       ebdb-default-country)
			     ebdb-country-list))))

    (cl-call-next-method
     class
     `(:streets ,streets
		:locality ,locality
		:object-name ,(plist-get slots :object-name)
		:region ,region
		:postcode ,postcode
		:country ,country)
     obj)))

(defun ebdb-edit-address-street (streets)
  "Edit list STREETS."
  (let ((n 0) street list)
    (condition-case nil
	(while t
	  (setq street
		(ebdb-read-string
		 (format "Street, line %d: " (1+ n))
		 (nth n streets) ebdb-street-list))
	  (push street list)
	  (setq n (1+ n)))
      ((ebdb-empty quit) nil))
    (reverse list)))

;; Addresses are displayed using `ebdb-format-address', which is
;; probably a candidate for refactoring.  In the meantime, provide a
;; stop-gap `ebdb-string' method for addresses that doesn't go through
;; the whole formatting routine, and additionally fits on one line.
;; This should probably get re-thought/re-worked later.

(cl-defmethod ebdb-string ((address ebdb-field-address))
  (ebdb-format-address address 2))

;;; Phone fields

(defclass ebdb-field-phone (ebdb-field-labeled ebdb-field)
  ((label-list :initform ebdb-phone-label-list)
   (country-code
    :initarg :country-code
    :type (or null number)
    :custom (choice (const :tag "Empty" nil)
		    (integer :tag "Country code"))
    :initform nil)
   (area-code
    :initarg :area-code
    :type (or null number)
    :custom (choice (const :tag "Empty" nil)
		    (integer :tag "Area code"))
    :initform nil)
   (number
    :initarg :number
    :type (or null string)
    :custom (choice (const :tag "Empty" nil)
		    (string :tag "Number"))
    :initform nil)
   (extension
    :initarg :extension
    :type (or null number)
    :custom (choice (const :tag "Empty" nil)
		    (integer :tag "Extension"))
    :initform nil)
   (actions
    :initform '(ebdb-field-phone-dial)))
  :human-readable "phone")

(cl-defmethod ebdb-string ((phone ebdb-field-phone))
  "Display the value of this phone number as a string."
  (with-slots (country-code area-code number extension) phone
    (let (outstring)
      (when extension
	(push (format "X%d" extension) outstring))
      (when number
	(let ((numstring (split-string number "" t)))
	  (push
	   (eval `(format ,(cl-case (length numstring)
			    (7
			     "%s%s%s-%s%s%s%s")
			    (8
			     "%s%s%s%s-%s%s%s%s")
			    (t
			     number))
			  ,@numstring))
	   outstring)))
      (when area-code
	(push (format "(%d) " area-code) outstring))
      (when country-code
	(push (format "+%d " country-code) outstring))
      (when outstring
	(apply #'concat outstring)))))

(cl-defmethod ebdb-read ((class (subclass ebdb-field-phone)) &optional slots obj)
  (let* ((country
	  (or (and obj
		   (slot-value obj 'country-code))
	      (plist-get slots 'country-code)))
	 (area
	  (or (and obj
		   (slot-value obj 'area-code))
	      (plist-get slots :area-code)))
	 (prompt
	  (concat "Number"
		  (when country
		    (format " +%d" country))
		  (when area
		    (format " (%d)" area))
		  ": "))
	 (default (when obj (slot-value obj 'number)))
	 (plist
	  (ebdb-error-retry
	   (ebdb-parse class
		       (ebdb-read-string prompt default)
		       slots))))
    (cl-call-next-method class plist obj)))

(cl-defmethod ebdb-parse ((_class (subclass ebdb-field-phone))
			  (string string)
			  &optional slots)
  "Parse a phone number from STRING and return a plist of
integers of the form \(country-code area-code number extension\).

The plist should be suitable for creating an instance of
`ebdb-field-phone'.

If plist SLOTS is present, allow values from that plist to
override parsing."
  ;; TODO: This `ebdb-parse' method returns a plist.  Other such
  ;; methods return an actual object.  We need consistency!
  (let ((country-regexp "\\+(?\\([0-9]\\{1,3\\}\\))?[ \t]+")
	(area-regexp "(?\\([0-9]\\{1,4\\}\\)[-)./ \t]+")
        (ext-regexp "[ \t]?e?[xX]t?\\.?[ \t]?\\([0-9]+\\)")
	acc)
    (with-temp-buffer
      (insert (ebdb-string-trim string))
      (goto-char (point-min))
      (unless (plist-member slots :country-code)
	(when (looking-at country-regexp)
	  (setq slots
		(plist-put slots :country-code (string-to-number (match-string 1))))
	  (goto-char (match-end 0))))
      (unless (plist-member slots :area-code)
	(when (looking-at area-regexp)
	  ;; Bit of a hack.  If we seem to have an area code, but there
	  ;; are fewer than six digits *after* the area code, assume
	  ;; it's not an area code at all, but part of the actual
	  ;; number.
	  (unless
	      (save-excursion
		(goto-char (match-end 0))
		(save-match-data
		  (< (abs (- (point)
			     (or (when (re-search-forward ext-regexp (point-max) t)
				   (goto-char (match-beginning 0))
				   (point))
				 (point-max))))
		     6)))
	    (setq slots
		  (plist-put slots :area-code (string-to-number (match-string 1))))
	    (goto-char (match-end 0)))))
      ;; There is no full regexp for the main phone number.  We just
      ;; chomp up any and all numbers that come after the area code,
      ;; until we hit an extension, or the end of the buffer.  All
      ;; phone slots but "number" are actually saved as numbers.  The
      ;; "number" is saved as a string, partially for ease in
      ;; formatting, partially because if it's too long Emacs turns it
      ;; into a float, which is a pain in the ass.
      (while (and (< (point) (point-max))
		  (null (looking-at-p ext-regexp))
		  (looking-at "[ \t]?\\([0-9]+\\)[- .]?"))
	(setq acc (concat acc (match-string-no-properties 1)))
	(goto-char (match-end 0)))
      (when (looking-at ext-regexp)
	(setq slots
	      (plist-put slots :extension (string-to-number
					   (match-string 1))))))
    (setq slots
	  (plist-put slots :number acc))
    slots))

;;; Notes field

(defclass ebdb-field-notes (ebdb-field)
  ((notes
    :type string
    :initarg :notes
    :initform ""
    :documentation "User notes on this contact."))
  :human-readable "notes")

(cl-defmethod ebdb-string ((notes ebdb-field-notes))
  (slot-value notes 'notes))

(cl-defmethod ebdb-read ((class (subclass ebdb-field-notes)) &optional slots obj)
  (let ((default (when obj (ebdb-string obj))))
    (cl-call-next-method class
			 (plist-put slots :notes (ebdb-read-string "Notes: " default))
			 obj)))

;;; Timestamp field

;; For both these fields, I'd actually prefer to store
;; seconds-since-epoch in the actual backend, for ease of use.  The
;; human-readable date string could be output in `ebdb-string'.

(defclass ebdb-field-timestamp (ebdb-field)
  ((timestamp
    :initarg :timestamp
    :type list
    :initform nil))
  :human-readable "timestamp")

(cl-defmethod ebdb-stamp-time ((field ebdb-field-timestamp))
  (ebdb-update-timestamp-field field 'timestamp))

(defun ebdb-update-timestamp-field (ts slot)
  (setf (slot-value ts slot) (current-time)))

(cl-defmethod ebdb-string ((field ebdb-field-timestamp))
  (format-time-string ebdb-time-format (slot-value field 'timestamp) t))

;;; Creation date field

;; This doesn't really do anything, but who knows who might want to
;; override it for their own nefarious purposes?

(defclass ebdb-field-creation-date (ebdb-field-timestamp)
  nil
  :human-readable "creation date")

;;; Anniversary field

;; The `ebdb-notice' method could let you know when you get an email
;; from someone and it happens to be their birthday.  The
;; `ebdb-action' method could open a calendar with point on the
;; upcoming anniversary, etc.

(defvar ebdb-anniversary-label-list '("birthday" "marriage" "death"))

(defclass ebdb-field-anniversary (ebdb-field-labeled ebdb-field-user)
  ((label-list :initform ebdb-anniversary-label-list)
   (date
    :initarg :date
    :type number
    :documentation
    "A number representing a date, as produced by calling
    `calendar-absolute-from-gregorian' on a gregorian date.")
   (calendar
    :initarg :calendar
    :type symbol
    :initform gregorian
    :documentation "The calendar to which this date applies.")
   (actions
    :initform '(ebdb-field-anniversary-browse)))
  :human-readable "anniversary")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-anniversary)) &optional slots obj)
  (require 'calendar)
  ;; The only unfortunate thing here is that we can't reasonably use
  ;; an existing date value as the default for entering a new one. Oh
  ;; well.
  (let ((date (calendar-absolute-from-gregorian
	       (calendar-read-date))))
    (cl-call-next-method class (plist-put slots :date date) obj)))

(cl-defmethod ebdb-string ((ann ebdb-field-anniversary))
  (require 'calendar)
  (calendar-date-string
   (calendar-gregorian-from-absolute (slot-value ann 'date))
   nil t))

;;; Id field

;; Used for recording an ID or tax id number.  Ie, national
;; identification numbers, SSNs, TINs, UTRs, and so on.

(defvar ebdb-id-label-list '("SSN" "TIN" "ID" "UTR")
  "List of known ID labels.")

(defclass ebdb-field-id (ebdb-field-labeled ebdb-field-obfuscated ebdb-field-user)
  ((label-list :initform ebdb-id-label-list)
   (id-number
    :type string
    :custom string
    :initarg :id-number
    :initform ""
    :documentation "The ID number itself."))
  :human-readable "id number")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-id)) &optional slots obj)
  (let ((id-number (ebdb-read-string "ID number: "
				     (when obj (slot-value obj 'id-number)))))
    (cl-call-next-method class (plist-put slots :id-number id-number) obj)))

(cl-defmethod ebdb-string ((field ebdb-field-id))
  (slot-value field 'id-number))

;;; Relationship field

;; This is a bit different from the organization role field, mostly
;; just meant for family relationships or similar things.

(defvar ebdb-relation-label-list '("father" "mother" "sister" "brother"
				   "son" "daughter" "aunt" "uncle"
				   "grandmother" "grandfather" "wife" "husband"))

(defclass ebdb-field-relation (ebdb-field-labeled ebdb-field)
  ((label-list :initform ebdb-relation-label-list)
   (rel-uuid
    :initarg :rel-uuid
    :type string
    :initform ""
    :documentation "The UUID of the target record.")
   (rel-label
    :initarg :rel-label
    :type string
    :custom string
    :initform ""
    :documentation "The label on the \"other side\" of the
    relation, pointing at this record."))
  :human-readable "relationship")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-relation)) &optional slots obj)
  (let* ((rec (ebdb-record-uuid (ebdb-prompt-for-record nil ebdb-default-record-class)))
	 (rel-label (ebdb-read-string "Reverse label (for the other record): "
				      nil ebdb-relation-label-list)))
    (setq slots (plist-put slots :rel-uuid rec))
    (setq slots (plist-put slots :rel-label rel-label))
    (cl-call-next-method class slots obj)))

(cl-defmethod ebdb-string ((rel ebdb-field-relation))
  (let ((rec (ebdb-gethash (slot-value rel 'rel-uuid) 'uuid)))
    (if rec
	(ebdb-string rec)
      "record not loaded")))

;; Image field

(defclass ebdb-field-image (ebdb-field)
  ((image
    :type (or null string symbol)
    :initarg :image))
  :human-readable "image")

(eieio-oset-default 'ebdb-field-image 'image ebdb-image)

(cl-defmethod ebdb-read ((image (subclass ebdb-field-image)) &optional slots obj)
  (let ((existing (when obj (slot-value obj 'image)))
	value)
    (setq value
	  (cond
	   ((or (stringp existing)
		(yes-or-no-p "Find image file? "))
	    (read-file-name "Image file: " ebdb-image-path existing 'confirm))
	   ((yes-or-no-p "Use name format? ")
	    (intern (completing-read "Format: " '(name fl-name lf-name) nil t existing)))
	   (t
	    (message "Image will be found using `ebdb-field-image-function'.")
	    nil)))
    (cl-call-next-method image (plist-put slots :image value) obj)))

(cl-defmethod ebdb-string ((field ebdb-field-image))
  (let ((image (slot-value field 'image)))
      (format (if (stringp image) "Image file: %s" "Image format: %s") image)))

;; URL field

(defvar ebdb-url-label-list '("homepage")
  "List of known URL labels.")

(defclass ebdb-field-url (ebdb-field-labeled ebdb-field-user)
  ((label-list
    :initform ebdb-url-label-list)
   (url
    :type string
    :initarg :url
    :custom string
    :initform "")
   (actions
    :initform '(ebdb-field-url-browse)))
  :human-readable "URL")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-url)) &optional slots obj)
  (let ((url (ebdb-read-string "Url: " (when obj (slot-value obj 'url)))))
    (cl-call-next-method class (plist-put slots :url url) obj)))

(cl-defmethod ebdb-string ((field ebdb-field-url))
  (slot-value field 'url))

;;; Fields that change EBDB's behavior.  Right now we've got
;;; `ebdb-mail-name' and the hard-coded 'name-format xfield that are
;;; meant to change a record's behavior.  I'm not convinced that
;;; that's the best way to handle that.  At the very least we need
;;; mail aliases, though.

(defclass ebdb-field-mail-alias (ebdb-field-user)
  ((alias
    :type string
    :initarg :alias
    :custom string
    :documentation
    "A string used as a mail alias for this record."))
  :human-readable "mail alias")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-mail-alias)) &optional slots obj)
  (let ((alias (ebdb-read-string "Alias: " (when obj (slot-value obj 'alias)))))
    (cl-call-next-method class (plist-put slots :alias alias) obj)))

(cl-defmethod ebdb-string ((field ebdb-field-mail-alias))
  (slot-value field 'alias))

;; TODO: Write `ebdb-init' and `ebdb-delete' methods for the
;; `ebdb-field-mail-alias' class.  These methods should do the work of
;; changing the defined mail aliases.

;; Passports

(defclass ebdb-field-passport (ebdb-field-user)
  ((country
    :type string
    :initarg :country
    :custom string
    :initform "")
   (number
    :type string
    :initarg :number
    :custom string
    :initform "")
   ;; TODO: issue-date and expiration-date should just be plain
   ;; strings, this is stupid.
   (issue-date
    :initarg :issue-date
    :type (or nil number))
   (expiration-date
    :initarg :expiration-date
    :type (or nil number)))
  :human-readable "passport")

(cl-defmethod ebdb-read ((class (subclass ebdb-field-passport)) &optional slots obj)
  (let ((country (ebdb-read-string "Country: " (when obj (slot-value obj 'country))))
	(number (ebdb-read-string "Number: " (when obj (slot-value obj 'number))))
	(issue-date (calendar-absolute-from-gregorian
		     (calendar-read-date)))
	(expiration-date (calendar-absolute-from-gregorian
			  (calendar-read-date))))
    (setq slots (plist-put slots :country country))
    (setq slots (plist-put slots :number number))
    (setq slots (plist-put slots :issue-date issue-date))
    (setq slots (plist-put slots :expiration-date expiration-date))
    (cl-call-next-method class slots obj)))

(cl-defmethod ebdb-string ((field ebdb-field-passport))
  (with-slots (country number) field
    (format "(%s) %s" country number)))

;;; Records

;; The basic, abstract `ebdb-record' class should require no user
;; interaction, and has no real user-facing fields (except for the
;; "fields" bucket, of course).  It takes care of all the fundamental
;; setup and housekeeping automatically.

(defclass ebdb-record (eieio-instance-tracker)
  ((uuid
    :initarg :uuid
    :type (or null ebdb-field-uuid)
    :initform nil)
   (tracking-symbol
    :initform ebdb-record-tracker)
   (creation-date
    :initarg :creation-date
    :type (or null ebdb-field-creation-date)
    :initform nil)
   (timestamp
    :initarg :timestamp
    :type (or null ebdb-field-timestamp)
    :initform nil)
   (fields
    :initarg :fields
    :type (list-of ebdb-field-user)
    :initform nil
    :documentation "This slot contains all record fields except
    those built-in to record subclasses.")
   (image
    :initarg :image
    :type (or null ebdb-field-image)
    :initform nil)
   (notes
    :initarg :notes
    :type (or null ebdb-field-notes)
    :initform nil
    :documentation "User notes for this contact.")
   (dirty
    :initarg :dirty
    :type boolean
    :initform nil
    :documentation "Does this record have changed fields?")
   (cache
    :initarg :cache
    :type (or null ebdb-cache)
    :initform nil
    ;:accessor ebdb-record-cache
    ))
  :abstract t
  :allow-nil-initform t
  :documentation "An abstract base class for creating EBDB
  records.")

(cl-defmethod ebdb-record-uuid ((record ebdb-record))
  (slot-value (slot-value record 'uuid) 'uuid))

(cl-defmethod ebdb-read ((class (subclass ebdb-record)) &optional slots)
  "Create a new record from the values collected into SLOTS."
  ;; All the other `ebdb-read' methods for record subclasses "bottom
  ;; out" here, and create a record.
  (let ((notes (ebdb-with-exit (ebdb-read ebdb-default-notes-class))))
    (when notes
      (setq slots (plist-put slots :notes notes)))
    (apply 'make-instance class slots)))

(cl-defmethod ebdb-delete ((record ebdb-record) &optional db unload)
  "Delete RECORD.

This goes through a series of deletion routines, removing RECORD
from its respective databases, un-hashing its uuid, running
`ebdb-delete' on its fields, etc."
  (let ((dbs (if db (list db)
	       (slot-value (ebdb-record-cache record) 'database)))
	(uuid (ebdb-record-uuid record)))
    ;; If DB is passed in, assume that it will be responsible for
    ;; calling `ebdb-db-remove-record'.
    (unless db
      (dolist (db dbs)
	(ebdb-db-remove-record db record)))
    (setq ebdb-seen-uuids
	  (delete uuid ebdb-seen-uuids))
    (dolist (field (slot-value record 'fields))
      (ebdb-delete field record unload))
    (ebdb-remhash uuid record)
    (delete-instance record)))

(cl-defmethod initialize-instance ((record ebdb-record) &optional slots)
  "Add a cache to RECORD."
  ;; This is the very first thing that happens to a record after it is
  ;; created (whether manually or loaded).
  (let ((cache (make-instance 'ebdb-cache)))
    (setq slots (plist-put slots :cache cache))
    (with-slots (timestamp creation-date) record
      (unless creation-date
	(setf creation-date (make-instance 'ebdb-field-creation-date))
	(ebdb-stamp-time creation-date))
      (unless timestamp
	(setf timestamp (make-instance 'ebdb-field-timestamp))
	(ebdb-stamp-time timestamp))
      (cl-call-next-method record slots))))

(cl-defmethod ebdb-init ((record ebdb-record))
  "Initiate a record after loading a database or creating a new
record."
  (dolist (field (ebdb-record-user-fields record))
    (ebdb-init field record))
  (ebdb-record-set-sortkey record))

(cl-defmethod ebdb-merge ((left ebdb-record)
			  (right ebdb-record)
			  &optional _auto)
  (with-slots (creation-date fields notes)
      right
    (with-slots ((lcreation-date creation-date)
		 (lfields fields)
		 (lnotes notes))
	left
      (let ((c-time (slot-value creation-date 'timestamp))
	    (lc-time (slot-value lcreation-date 'timestamp)))
	;; We're merging all values into LEFT.
	(setf (slot-value left 'creation-date)
	      (if (or (equal lc-time c-time)
		      (time-less-p c-time lc-time))
		  creation-date
		lcreation-date))
	(ebdb-stamp-time left)
	(setf (slot-value left 'fields)
	      (delete-dups
	       (append fields lfields)))
	(when (or notes lnotes)
	  (setf (slot-value left 'notes)
		(mapconcat #'identity (list notes lnotes) "\n\n"))))))
  left)

(cl-defmethod ebdb-stamp-time ((record ebdb-record))
  (ebdb-stamp-time (slot-value record 'timestamp)))

(cl-defmethod ebdb-record-change-field ((record ebdb-record) (old-field ebdb-field) &optional new-field)
  "Change the values of FIELD belonging to RECORD."
  (let* ((fieldclass (eieio-object-class old-field))
	 (slot (car (ebdb-record-field-slot-query
		     (eieio-object-class record)
		     (cons nil fieldclass))))
	 (new-field (or new-field (ebdb-read fieldclass nil old-field))))
    (when (or (null (equal old-field new-field))
	      ebdb-update-unchanged-records)
      (ebdb-record-delete-field record slot old-field)
      (ebdb-record-insert-field record slot new-field)
      new-field)))

(cl-defmethod ebdb-record-insert-field ((record ebdb-record) slot field)
  "Add FIELD to RECORD's SLOT."
  ;; First, the databases "actually" add the field to the record, ie
  ;; persistence.  The rest of this method is just updating the
  ;; existing record instance with the new field.
  (dolist (db (slot-value (ebdb-record-cache record) 'database))
    (when field
      (setq field (ebdb-db-add-record-field db record slot field))))
  (when field
    (condition-case nil
	(object-add-to-list record slot field)
      (invalid-slot-type
       (setf (slot-value record slot) field)))
    (ebdb-init field record))
  field)

(cl-defmethod ebdb-record-delete-field ((record ebdb-record) slot field)
  "Delete FIELD from RECORD's SLOT, or set SLOT to nil, if no FIELD."
  ;; We don't use `slot-makeunbound' because that's a huge pain in the
  ;; ass, and why would anyone want those errors?
  (dolist (db (slot-value (ebdb-record-cache record) 'database))
    (ebdb-db-remove-record-field db record slot field))
  (if (listp (slot-value record slot))
      (object-remove-from-list record slot field)
    (setf (slot-value record slot) nil))
  (ebdb-delete field record))

(cl-defgeneric ebdb-record-field-slot-query (record-class &optional query alist)
  "Ask RECORD-CLASS for information about its interactively-settable fields.

If QUERY is nil, simply return ALIST, which is a full list of
acceptable fields.  Each list element is a cons of the form (SLOT
. FIELDCLASS), meaning that RECORD-CLASS can accept fields of
class FIELDCLASS in SLOT.

If QUERY is non-nil, it should be a cons of either '(SLOT . nil),
or '(nil . FIELDCLASS).  The \"nil\" is the value to query for:
either \"which slot can accept this field class\", or \"which
fieldclass is appropriate for this slot\".  The return value in
either case is a cons with both slot and fieldclass filled in.")

(cl-defmethod ebdb-record-field-slot-query ((class (subclass ebdb-record))
					    &optional query alist)
  (let ((alist (append
		'((notes . ebdb-field-notes)
		  (image . ebdb-field-image))
		alist))
	user-class)
    ;; Pick up all externally-defined user fields.
    (dolist (f (eieio-build-class-alist 'ebdb-field-user t))
      (setq user-class (intern (car f)))
      (unless (rassq user-class alist)
	(push (cons 'fields user-class) alist)))
    ;; Look, Ma, I used pcase!
    (pcase query
      (`(nil . ,cls)
       (or (rassq cls alist)
	   (rassq (ebdb-class-in-list-p class (mapcar #'cdr alist))
		  alist)
	   (signal 'ebdb-unacceptable-field (list cls))))
      (`(,slot . nil)
       (or (assq slot alist)
	   (signal 'ebdb-unacceptable-field (list slot))))
      (_ alist))))

(cl-defgeneric ebdb-record-current-fields (record &optional f-list all)
  "Return an alist of all RECORD's current fields.

Each element of the alist is a cons of (slot-name
. field-instance), where slot-name is a symbol, and
field-instance is an instance of a subclass of `ebdb-field'.
These conses are collected in F-LIST.

If ALL is non-nil, really return all of RECORD's fields.  If nil,
only return fields that are suitable for user editing.")

(cl-defmethod ebdb-record-current-fields ((record ebdb-record)
					  &optional f-list all)
  "This is the \"bottom-most\" implementation of this method."
  (with-slots (fields image timestamp creation-date uuid notes) record
    (dolist (f fields)
      (push `(fields . ,f) f-list))
    (when image
      (push `(image . ,image) f-list))
    (when all
      (push `(timestamp . ,timestamp) f-list)
      (push `(creation-date . ,creation-date) f-list)
      (push `(uuid . ,uuid) f-list))
    (when notes
      (push (cons 'notes notes) f-list)))
  f-list)

(cl-defmethod ebdb-notice ((_rec ebdb-record))
  ;; Implement this later.
  t)

(cl-defmethod ebdb-record-search ((record ebdb-record)
				  (_type (eql name))
				  (regexp string))
  (or (string-match-p regexp (or (ebdb-record-name record) ""))
      (seq-find
       (lambda (n)
	 (string-match-p regexp n))
       (ebdb-record-alt-names record))
      (ebdb-field-search (slot-value record 'name) regexp)))

(cl-defmethod ebdb-record-search ((record ebdb-record)
				  (_type (eql notes))
				  (regexp string))
  (if-let (notes (slot-value record 'notes))
      (string-match-p regexp (ebdb-string notes))))

(cl-defmethod ebdb-record-search ((record ebdb-record)
				  (_type (eql user))
				  search-clause)
  (pcase search-clause
    (`(* . ,(and regexp (pred stringp)))
     ;; check all user fields
     (catch 'found
       (dolist (f (ebdb-record-user-fields record))
	 (when (or (string-match-p regexp (ebdb-string f))
		   (and (slot-exists-p f 'object-name)
			(string-match-p regexp (slot-value f 'object-name))))
	   (throw 'found t)))
       ;; so that "^$" can be used to find records that
       ;; have no notes
       (when (string-match-p regexp "")
	 (throw 'found t))))
    (`((,(and field-string (pred stringp)) . ,(and class (pred symbolp))) . ,(and regexp (pred stringp))) ; check one field
     (catch 'found
       (if (eql class 'ebdb-field-class-simple)
	   (when (string-match-p
		  regexp (ebdb-string
			  (ebdb-record-user-field record field-string)))
	     (throw 'found t))
	 (dolist (f (ebdb-record-user-fields record))
	   ;; If it's not a `ebdb-field-class-simple', the
	   ;; "field-string" is always going to be the same.  Just
	   ;; check if the regexp matches either the label, if there
	   ;; is one, or the value.
	   (when (and (object-of-class-p f class)
		      (or (and (slot-exists-p f 'object-name)
			       (string-match-p regexp (slot-value f 'object-name)))
			  (string-match-p regexp (ebdb-string f))))
	     (throw 'found t))))))
    (_ nil)))

;; TODO: rename this to `ebdb-record-name-string', it's confusing.
(cl-defmethod ebdb-record-name ((record ebdb-record))
  (slot-value (ebdb-record-cache record) 'name-string))

(cl-defmethod ebdb-record-alt-names ((record ebdb-record))
  (slot-value (ebdb-record-cache record) 'alt-names))

(cl-defmethod ebdb-record-related ((_record ebdb-record)
				   (_field ebdb-field))
  "Provide a base method that does nothing."
  nil)

(cl-defmethod object-print ((record ebdb-record) &optional strings)
  (cl-call-next-method
   record
   (append strings (format " %s" (if (ebdb-record-cache record)
				     (ebdb-record-name record)
				   (ebdb-string (slot-value record 'name)))))))

;; The following functions are here because they need to come after
;; `ebdb-record' has been defined.

(cl-defmethod ebdb-field-image-get ((field ebdb-field-image) (record ebdb-record))
  "Return the image for image field FIELD.

This function returns an actual image, suitable for display with
`insert-image'."
  (let* ((image-slot (slot-value field 'image))
	 (image
	  (cond ((stringp image-slot)
		 image-slot)
		((eq image-slot 'name)
		 (ebdb-string record))
		(t
		 (ebdb-field-image-function ebdb-image record)))))
    (when image
      (create-image
       (if (stringp image)
	   (if (file-name-absolute-p image)
	       image
	     (locate-file image ebdb-image-path
			  ebdb-image-suffixes))
	 image)))))

(cl-defmethod ebdb-field-image-function ((_field ebdb-field-image) (_record ebdb-record))
  "Return image data for RECORD from image field FIELD.

The return value of this function will be passed to
`create-image', which see.  It can either be an image file name,
or actual image data."
  ;; Unimplemented.
  nil)

(cl-defmethod ebdb-field-phone-dial ((_record ebdb-record)
				     (phone ebdb-field-phone))
  "Make some attempt to call this PHONE number."
  ;; This won't actually work.
  (ebdb-dial-number (ebdb-string phone)))

(cl-defmethod ebdb-field-url-browse ((_record ebdb-record)
				     (field ebdb-field-url))
  (browse-url (slot-value field 'url)))

(cl-defmethod ebdb-field-anniversary-browse ((_record ebdb-record)
					     (_field ebdb-field-anniversary))
  (require 'calendar)
  (message "This isn't done yet."))

;;; `ebdb-record' subclasses

(defclass ebdb-record-entity (ebdb-record)
  ((mail
    :initarg :mail
    :type (list-of ebdb-field-mail)
    :initform nil)
   (phone
    :initarg :phone
    :type (list-of ebdb-field-phone)
    :initform nil)
   (address
    :initarg :address
    :type (list-of ebdb-field-address)
    :initform nil))
  :allow-nil-initform t
  :abstract t
  :documentation "An abstract class representing basic entities
  that have mail, phone and address fields.")

(cl-defmethod ebdb-init ((record ebdb-record-entity))
  (dolist (phone (slot-value record 'phone))
    (ebdb-init phone record))
  (dolist (mail (slot-value record 'mail))
    (ebdb-init mail record))
  (dolist (address (slot-value record 'address))
    (ebdb-init address record))
  (cl-call-next-method))

;; `ebdb-read' is only called for records on first creation, so we
;; don't have to worry about existing objects.

(cl-defmethod ebdb-read ((class (subclass ebdb-record-entity)) &optional slots)
  "Prompt for the basic slot values of `ebdb-record-entity'."
  (let ((mail (ebdb-loop-with-exit
	       (ebdb-read ebdb-default-mail-class)))
	(phone (ebdb-loop-with-exit
		(ebdb-read ebdb-default-phone-class)))
	(address (ebdb-loop-with-exit
		  (ebdb-read ebdb-default-address-class))))
    (setq slots (plist-put slots :mail mail))
    (setq slots (plist-put slots :phone phone))
    (setq slots (plist-put slots :address address))
    (cl-call-next-method class slots)))

(cl-defmethod ebdb-delete ((record ebdb-record-entity) &optional _db unload)
  (dolist (mail (slot-value record 'mail))
    (ebdb-delete mail record unload))
  (dolist (field (ebdb-record-user-fields record))
    (ebdb-delete field record unload))
  ;; Maybe should also be calling `ebdb-delete' on the phone and
  ;; address fields, just in case.
  (cl-call-next-method))

(cl-defmethod ebdb-merge ((left ebdb-record-entity)
			  (right ebdb-record-entity)
			  &optional auto)
  (with-slots (mail phone address) right
    (with-slots ((lmail mail) (lphone phone) (laddress address)) left
      (setf (slot-value left 'mail) (delete-dups (append mail lmail)))
      (setf (slot-value left 'phone) (delete-dups (append phone lphone)))
      (setf (slot-value left 'address) (delete-dups (append address laddress)))))
  (cl-call-next-method left right auto))

(cl-defmethod ebdb-record-field-slot-query ((class (subclass ebdb-record-entity)) &optional query alist)
  (cl-call-next-method
   class
   query
   (append
    `((mail . ebdb-field-mail)
      (phone . ebdb-field-phone)
      (address . ebdb-field-address))
    alist)))

(cl-defmethod ebdb-record-current-fields ((record ebdb-record-entity)
					  &optional f-list all)
  (with-slots (mail phone address) record
    (dolist (m mail)
      (push `(mail . ,m) f-list))
    (dolist (p phone)
      (push `(phone . ,p) f-list))
    (dolist (a address)
      (push `(address . ,a) f-list)))
  (cl-call-next-method record f-list all))

(cl-defmethod ebdb-record-change-name ((record ebdb-record-entity) name)
  (when (slot-value record 'name)
    (ebdb-record-delete-field record 'name (slot-value record 'name)))
  (ebdb-record-insert-field record 'name name))

(cl-defmethod ebdb-record-organizations ((_record ebdb-record-entity))
  nil)

(cl-defmethod ebdb-record-insert-field :after ((record ebdb-record-entity)
					       _slot
					       (_mail ebdb-field-mail))
  "After giving RECORD a new mail field, sort RECORD's mails by
priority."
  (let ((sorted (ebdb-sort-mails (slot-value record 'mail))))
    (setf (slot-value record 'mail) sorted)))

(cl-defmethod ebdb-record-primary-mail ((record ebdb-record-entity))
  "Return the primary mail field of RECORD."
  (let ((mails (ebdb-record-mail record t)))
    (object-assoc 'primary 'priority mails)))

(cl-defmethod ebdb-record-search ((record ebdb-record-entity)
				  (_type (eql phone))
				  (regexp string))
  (let ((phones (ebdb-record-phone record)))
    (if phones
	(catch 'found
	  (dolist (ph phones)
	    (when (ebdb-field-search ph regexp)
	      (throw 'found t))))
      (string-match-p regexp ""))))

(cl-defmethod ebdb-record-search ((record ebdb-record-entity)
				  (_type (eql address))
				  (regexp string))
  (let ((adds (ebdb-record-address record)))
    (if adds
	(catch 'found
	  (dolist (a adds)
	    (when (ebdb-field-search a regexp)
	      (throw 'found t))))
      (string-match-p regexp ""))))

(cl-defmethod ebdb-record-search ((record ebdb-record-entity)
				  (_type (eql mail))
				  (regexp string))
  (let ((mails (ebdb-record-mail record t)))
    (if mails
	(catch 'found
	  (dolist (m mails)
	    (when (ebdb-field-search m regexp)
	      (throw 'found t))))
      (string-match-p regexp ""))))

;; TODO: There's no reason why the aka slot can't belong to
;; `ebdb-record-entity'.  In fact, what we ought to do is put both the
;; 'name and the 'aka slots on `ebdb-record-entity', and have both
;; slot types set to `ebdb-field-name'.  Then provide a fairly simple
;; mechanism for letting the user choose whether a name/aka should be
;; simple or complex.  Or, when creating or parsing name fields, we
;; could always start out with the `ebdb-parse' and `ebdb-read'
;; methods of `ebdb-field-name', which could then dispatch to the
;; simple/complex methods depending on the initial string values.  Or
;; something like that.

(defclass ebdb-record-person (ebdb-record-entity)
  ((name
    :initarg :name
    :type (or null ebdb-field-name-complex)
    :initform nil)
   (aka
    :initarg :aka
    :type (list-of ebdb-field-name)
    :initform nil
    :accessor ebdb-record-aka)
   (relations
    :initarg :relations
    :type (list-of ebdb-field-relation)
    :initform nil)
   (organizations
    :initarg :organizations
    :type (list-of ebdb-field-role)
    :initform nil))
  :allow-nil-initform t
  :documentation "A record class representing a person.")

(cl-defmethod ebdb-string ((record ebdb-record-person))
  "Return a readable string label for RECORD."
  (slot-value (ebdb-record-cache record) 'name-string))

(cl-defmethod ebdb-read ((class (subclass ebdb-record-person)) &optional slots)
  "Read the name slot for a \"person\" record."
  (let* ((name (ebdb-read ebdb-default-name-class (plist-get slots :name))))
    (cl-call-next-method
     class (plist-put slots :name name))))

(cl-defmethod ebdb-init ((record ebdb-record-person))
  (let ((name (slot-value record 'name)))
    (ebdb-init name record)
    (setf (slot-value (ebdb-record-cache record) 'name-string) (ebdb-string name)))
  (dolist (aka (slot-value record 'aka))
      (ebdb-init aka record))
  (dolist (relation (slot-value record 'relations))
    (ebdb-init relation record))
  (dolist (role (slot-value record 'organizations))
    (ebdb-init role record))
  (cl-call-next-method))

(cl-defmethod ebdb-delete ((record ebdb-record-person) &optional _db unload)
  (ebdb-delete (slot-value record 'name) record unload)
  (dolist (a (slot-value record 'aka))
    (ebdb-delete a record unload))
  (dolist (r (slot-value record 'relations))
    (ebdb-delete r record unload))
  (dolist (o (slot-value record 'organizations))
    (ebdb-delete o record unload))
  (cl-call-next-method))

(cl-defmethod ebdb-merge ((left ebdb-record-person)
			  (right ebdb-record-person)
			  &optional auto)
  "Merge person RIGHT into LEFT, and return LEFT."
  (with-slots (name
	       aka
	       relations
	       organizations)
      right
    (with-slots ((lname name)
		 (laka aka)
		 (lrelations relations)
		 (lorganizations organizations))
	left

      (if auto
	  (object-add-to-list left 'aka name)

	(if (yes-or-no-p (format "Make %s the primary name? " (ebdb-record-name right)))
	    (progn
	      (ebdb-record-change-name left name)
	      (when (yes-or-no-p (format "Keep %s as an aka? " (ebdb-record-name left)))
		(object-add-to-list left 'aka lname)))
	  (when (yes-or-no-p (format "Keep %s as an aka? " (ebdb-record-name right)))
	    (object-add-to-list left 'aka name))))

      (setf (slot-value left 'relations)
	    (delete-dups (append relations lrelations)))
      (setf (slot-value left 'organizations)
	    (delete-dups (append organizations lorganizations)))))
  (cl-call-next-method left right auto))

(cl-defmethod ebdb-record-field-slot-query ((class (subclass ebdb-record-person)) &optional query alist)
  (cl-call-next-method
   class
   query
   (append
    '((aka . ebdb-field-name-complex)
      (relations . ebdb-field-relation)
      (organizations . ebdb-field-role))
    alist)))

(cl-defmethod ebdb-record-firstname ((rec ebdb-record-person) &optional full)
  (when (slot-value rec 'name)
    (ebdb-name-given (slot-value rec 'name) full)))

(cl-defmethod ebdb-record-lastname ((rec ebdb-record-person))
  (when (slot-value rec 'name)
    (ebdb-name-last (slot-value rec 'name))))

(cl-defmethod ebdb-record-current-fields ((record ebdb-record-person)
					  &optional f-list all)
  (with-slots (name aka relations organizations) record
    (push `(name . ,name) f-list)
    (dolist (a aka)
      (push `(aka . ,a) f-list))
    (dolist (r relations)
      (push `(relations . ,r) f-list))
    (dolist (o organizations)
      (push `(organizations . ,o) f-list)))
  (cl-call-next-method record f-list all))

;; TODO: Now that both people and organizations have class-based
;; names, there's no longer any real reason to have two
;; implementations of this method.  One implementation that accepted a
;; `ebdb-field-name' subclass symbol would be sufficient.
(cl-defmethod ebdb-record-change-name ((record ebdb-record-person) &optional name)
  (let* ((new-name (or name (ebdb-read ebdb-default-name-class nil
				       (slot-value record 'name)))))
    (setf (slot-value (ebdb-record-cache record) 'name-string) (ebdb-string new-name))
    (cl-call-next-method record new-name)))

(cl-defmethod ebdb-record-adopt-role-fields ((record ebdb-record-person) &optional _prompt)
  "Go through all of RECORDs fields and see if any of them should
be moved to an organization role.

Currently only works for mail fields."
  (let ((roles (slot-value record 'organizations))
	org domain)
    (dolist (r roles)
      (setq org (ebdb-gethash (slot-value r 'org-uuid) 'uuid))
      (dolist (m (ebdb-record-mail record))
	(setq domain (cadr (split-string (slot-value m 'mail) "@")))
	(when (and domain
		   (string-match-p domain
				   (slot-value org 'domain))
		   (yes-or-no-p (format "Move mail %s to organization %s? "
					(ebdb-string m)
					(ebdb-string org))))
	  (setf (slot-value r 'mail) m)
	  (ebdb-record-delete-field record 'mail m))))))

(cl-defmethod ebdb-record-related ((_record ebdb-record-person)
				   (field ebdb-field-relation))
  (ebdb-gethash (slot-value field 'rel-uuid) 'uuid))

(cl-defmethod ebdb-record-related ((_record ebdb-record-person)
				   (field ebdb-field-role))
  (ebdb-gethash (slot-value field 'org-uuid) 'uuid))

(cl-defmethod ebdb-record-organizations ((record ebdb-record-person))
  "Return a list of organization string names from RECORD's cache."
  (slot-value (ebdb-record-cache record) 'organizations))

;;; This needs some more thought.
;; (cl-defmethod ebdb-mail-set-priority ((mail ebdb-field-mail)
;; 				      (record ebdb-record-person)
;; 				      &optional priority)
;;   "Set the priority level for the MAIL address of RECORD to
;; PRIORITY, if given, or prompt for priority."
;;   ;; How do we get the symbol choices back out of the :custom
;;   ;; declaration?  Or is there a better way to keep the values in some
;;   ;; other central location?
;;   (let* ((level (or priority
;; 		    (intern
;; 		     (completing-read "Set priority to: "
;; 				      '(normal primary defunct) nil t
;; 				      (slot-value mail 'priority) ))))
;; 	 (new-mail (clone mail :priority level))
;; 	 orig-primary)

;;     (when (eq level 'primary)
;;       (setq
;;        ;; This is horrible
;;        orig-primary
;;        (or
;; 	(dolist (m (slot-value record 'mail) orig-primary)
;; 	  (when (eq (slot-value m 'priority) 'primary)
;; 	    (setq orig-primary (cons 'rec m))))
;; 	(unless orig-primary
;; 	  (dolist (r (slot-value record 'organizations) orig-primary)
;; 	    (when (eq (slot-value
;; 		       (slot-value r 'mail)
;; 		       'priority)
;; 		      'primary)
;; 	      (setq orig-primary (cons 'role (slot-value r 'mail)))))))))

;;     (when (or (null orig-primary)
;; 	      (yes-or-no-p (format "Switch primary address to %s? " (ebdb-string new-mail))))
;;       (if role
;; 	  (let ((new-role (clone role :mail new-mail)))
;; 	    (ebdb-record-change-field record role new-role))
;; 	(ebdb-record-change-field record mail new-mail))
;;       ;; Unset the original primary address
;;       (when orig-primary
;; 	(let ((unset-mail (clone (cdr orig-primary) :priority 'normal)))
;; 	 (if (eq (car orig-primary) 'rec)
;; 	     (ebdb-record-change-field record mail unset-mail)
;; 	   (bbbd-record-change-field
;; 	    record role (clone role :mail unset-mail))))))))

;;; other record subclasses.

(defclass ebdb-field-domain (ebdb-field)
  ((domain
    :initarg :domain
    :type string
    :initform ""
    :documentation))
  :human-readable "domain"
  :documentation "An organization's domain name.  Useful for
  automatically constructing a homepage for the organization, or
  email addresses for member person records.")

(cl-defmethod ebdb-read ((domain (subclass ebdb-field-domain)) &optional slots obj)
  (cl-call-next-method
   domain
   (plist-put slots :domain
	      (ebdb-read-string "Domain: "
				(when obj (slot-value obj 'domain))))
   obj))

(cl-defmethod ebdb-string ((domain ebdb-field-domain))
  (slot-value domain 'domain))

(defclass ebdb-record-organization (ebdb-record-entity)
  ((name
    :initarg :name
    :type ebdb-field-name-simple
    :initform nil
    :documentation "The name of this organization.")
   (domain
    :initarg :domain
    :type (or null ebdb-field-domain)
    :initform nil
    :documentation "The base domain name for this organization.
    This can be used to open the organization's, and also as a
    default for email addresses of people with roles tied to this
    organization."))
  :allow-nil-initform t
  :documentation "A record class representing an organization.")

(cl-defmethod ebdb-init ((record ebdb-record-organization))
  (let ((name (slot-value record 'name)))
    (ebdb-init name record)
    (setf (slot-value (ebdb-record-cache record) 'name-string)
	  (ebdb-string name))
    (cl-call-next-method)))

(cl-defmethod ebdb-delete ((org ebdb-record-organization) &optional _db unload)
  (let* ((uuid (ebdb-record-uuid org))
	 (org-entry (gethash uuid ebdb-org-hashtable))
	 record)
    (remhash uuid ebdb-org-hashtable)
    (when (and org-entry
	       (null unload)
    	       (yes-or-no-p (format "Delete all roles associated with %s"
    				    (ebdb-string org))))
      (dolist (r org-entry)
    	(setq record (ebdb-gethash (car r) 'uuid))
    	(ebdb-record-delete-field record 'organizations (cdr r))))
    (cl-call-next-method)))

(cl-defmethod ebdb-string ((record ebdb-record-organization))
  "Return a string representation of RECORD."
  (slot-value (ebdb-record-cache record) 'name-string))

(cl-defmethod ebdb-read ((class (subclass ebdb-record-organization)) &optional slots)
  (let ((name (ebdb-read 'ebdb-field-name-simple slots
			 (plist-get slots 'name)))
	(domain (ebdb-with-exit (ebdb-read 'ebdb-field-domain))))
    (setq slots (plist-put slots :name name))
    (when domain
      (setq slots (plist-put slots :domain domain)))
    (cl-call-next-method class slots)))

(cl-defmethod ebdb-merge ((left ebdb-record-organization)
			  (right ebdb-record-organization)
			  &optional auto)
  "Merge organization RIGHT into LEFT, and return LEFT."
  (with-slots (name domain) right
    (when (or auto (yes-or-no-p (format "Use name %s? " (ebdb-string name))))
      (ebdb-record-change-name left name))
    (when (and domain
	       (or auto (yes-or-no-p (format "Use domain %s? " domain))))
      (setf (slot-value left 'domain) domain)))
  (cl-call-next-method))

(cl-defmethod ebdb-record-field-slot-query ((class (subclass ebdb-record-organization))
					    &optional query alist)
  (cl-call-next-method
   class
   query
   (append
    '((domain . ebdb-field-domain))
    alist)))

(cl-defmethod ebdb-record-current-fields ((record ebdb-record-organization)
					  &optional f-list all)
  (with-slots (name domain) record
    (push `(name . ,name) f-list)
    (when domain
      (push `(domain . ,domain) f-list)))
  (cl-call-next-method record f-list all))

(cl-defmethod ebdb-record-search ((record ebdb-record-organization)
				  (_type (eql name))
				  (regex string))
  (string-match-p regex (ebdb-string (slot-value record 'name))))

(cl-defmethod ebdb-record-insert-field :after ((org ebdb-record-organization)
					       _slot
					       (field ebdb-field-domain))
  (let ((roles (gethash (ebdb-record-uuid org) ebdb-org-hashtable))
	(domain (slot-value field 'domain))
	rec)
    (dolist (r roles)
      (setq rec (ebdb-gethash (car r) 'uuid))
      (dolist (m (ebdb-record-mail rec))
	(when (and (string-match-p domain (slot-value m 'mail))
		   (yes-or-no-p (format "Move address %s of %s to %s role? "
					(ebdb-string m)
					(ebdb-string rec)
					(ebdb-string org))))
	  (setf (slot-value (cdr r) 'mail) m)
	  (ebdb-record-delete-field
	   rec
	   (car (ebdb-record-field-slot-query
		 (eieio-object-class rec)
		 `(nil . ,(eieio-object-class m))))
	   m))))))

(cl-defmethod ebdb-record-change-field ((_record ebdb-record-organization)
					(old-field ebdb-field-role)
					&optional new-field)
  "Change the values of FIELD belonging to RECORD.

This method exists to allow users to edit a role field from an
organization record.  It switches the record being edited to the
appropriate person record."
  (let ((record (ebdb-gethash (slot-value old-field 'record-uuid) 'uuid)))
    (cl-call-next-method record old-field new-field)))

(cl-defmethod ebdb-record-delete-field ((_record ebdb-record-organization)
					slot
					(field ebdb-field-role))
  (let ((record (ebdb-gethash (slot-value field 'record-uuid) 'uuid)))
    (cl-call-next-method record slot field)))

(cl-defmethod ebdb-record-insert-field :after ((record ebdb-record-person)
					       _slot
					       (field ebdb-field-role))
  (let* ((org-uuid (slot-value field 'org-uuid))
	 (org (ebdb-gethash org-uuid 'uuid))
	 (org-domain (slot-value org 'domain))
	 (role-mail (slot-value field 'mail)))
    (when (and org-domain (not role-mail))
      (dolist (m (ebdb-record-mail record t))
	(when (and (string-match-p (slot-value org-domain 'domain) (ebdb-string m))
		   (yes-or-no-p (format "Move address %s to %s role? "
					(ebdb-string m) (ebdb-string org))))
	  (setf (slot-value field 'mail) m)
	  (ebdb-record-delete-field
	   record
	   (car (ebdb-record-field-slot-query
		 (eieio-object-class record)
		 `(nil . ,(eieio-object-class m))))
	   m))))))

(cl-defmethod ebdb-record-change-name ((org ebdb-record-organization) &optional name)
  (let ((new-name (or name (ebdb-read 'ebdb-field-name-simple nil (slot-value org 'name)))))
    (setf (slot-value (ebdb-record-cache org) 'name-string) (ebdb-string new-name))
    (cl-call-next-method org new-name)))

(cl-defmethod ebdb-record-related ((_record ebdb-record-organization)
				   (field ebdb-field-role))
  (ebdb-gethash (slot-value field 'record-uuid) 'uuid))

(defun ebdb-record-add-org-role (record org &optional mail fields)
  "Convenience function for creating a role relationship between RECORD and ORG.

MAIL and/or FIELDS, if present, should be a list of field
instances to add as part of the role."
  (let ((role (make-instance 'ebdb-field-role
			     :org-uuid (ebdb-record-uuid org)
			     :record-uuid (ebdb-record-uuid record))))
    (when fields
      (dolist (f fields)
	(object-add-to-list role 'fields f)))
    (when mail
      (setf (slot-value role 'mail) mail))
    (ebdb-record-insert-field record 'organizations role)
    (ebdb-init role record)))

(defclass ebdb-record-mailing-list (ebdb-record eieio-named)
  ((name
    :type ebdb-field-name-simple
    :initarg :name
    :initform nil))
  :allow-nil-initform t
  :documentation "A record class representing a mailing list.")

(cl-defmethod ebdb-read ((_class (subclass ebdb-record-mailing-list)) &optional _db _slots)
  (error "Mailing list records haven't been implemented yet."))

;;; Merging

;; There should be two kinds of merging: interactive and automatic.
;; Interactive merging is for when you actually have duplicate records
;; for the same entity -- this is just a mistake, and should be
;; handled interactively.  Automatic merging happens when you've put a
;; single record in multiple databases, and they've gotten out of
;; sync.  The merging process detects this by checking the 'uuid and
;; 'timestamp

(defun ebdb-check-uuid (uuid)
  "Ensure that UUID hasn't been seen before.  If it has, raise an
error containing the record that already has that uuid."
  (when (member uuid ebdb-seen-uuids)
    (signal 'ebdb-duplicate-uuid
	    (list (ebdb-gethash uuid 'uuid)))))

(defun ebdb-make-uuid (&optional prefix)
  "Create and return a new UUID.

This depends on the value of `ebdb-uuid-function'.  When that
variable is a string, assume the string refers to a system
executable.  When a symbol, assume an Elisp function."
  (let ((prefix-string (unless (string-empty-p prefix)
			 (concat prefix "-")))
	(uid
	 (cond
	  ((stringp ebdb-uuid-function)
	   (shell-command-to-string
	    (executable-find ebdb-uuid-function)))
	  ((functionp ebdb-uuid-function)
	   (funcall ebdb-uuid-function)))))
    (concat prefix-string
	    (replace-regexp-in-string
	     "[\n\t ]+" ""
	     uid))))

;;; The database class(es)

(defclass ebdb-db (eieio-named eieio-persistent)
  ;; Is there a need for a "remote" slot?
  ((uuid
    :initarg :uuid
    :initform nil
    :type (or null ebdb-field-uuid)
    :documentation
    "A unique identifier for this database.")
   (file-header-line
    :initform ";; EBDB file-persistent database")
   (sync-time
    :type (or null cons)
    :initarg :sync-time
    :initform nil
    :documentation "The date/time at which this database was last synced with its source.")
   (records
    :initarg :records
    :initform nil
    :type (list-of ebdb-record)
    :documentation
    "The records stored in this database.")
   (read-only
    :initarg :read-only
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Is this database read-only?")
   (version
    :initarg :version
    :type string
    :initform "0.1"
    :documentation
    "The version number of this database.")
   (uuid-prefix
    :initarg :uuid-prefix
    :type string
    :initform ""
    :custom string
    :documentation
    "A string prefix to be added to all UUIDs generated for
    records created in this database.")
   (buffer-char
    :initarg :buffer-char
    :type string
    :initform ""
    :custom string
    :documentation
    "A single character used in the *EBDB* buffer to indicate the
    database(s) to which a record belongs.")
   (dirty
    :initarg :dirty
    :initform nil
    :type boolean
    :documentation
    "Set to t when the database has unsaved data.")
   (auto-save
    :initarg :auto-save
    :initform t
    :type boolean
    :custom boolean
    :documentation
    "Set to t to have this database auto save itself.  Databases
    that are disabled or read-only will not be saved.")
   (disabled
    :initarg :disabled
    :initform nil
    :type boolean
    :custom boolean
    :documentation
    "Set to t to temporarily disable this database.  Records will
    not be loaded from or saved to it.")
   (record-class
    :initarg :record-class
    ;; I don't think I can actually set this to `ebdb-record': the
    ;; type needs to be a class, not an instance.  Can I do that?
    :type symbol
    :initform nil
    :custom symbol
    :documentation
    "The default EIEIO class for records in this database.  Must
    be a subclass of `ebdb-record'."))

  "The base class for the EBDB database store.  This class should
not be instantiated directly, subclass it instead."
  :allow-nil-initform t
  :abstract t)

;; I was told not to use this in Gnus, but I don't remember why.  I
;; suspect it was backward compatibility, and that's obviously already
;; out the window.
(oset-default 'ebdb-db record-class ebdb-default-record-class)

(cl-defmethod initialize-instance ((db ebdb-db) &optional slots)
  "Make sure DB has a uuid."
  (unless (and (slot-boundp db 'uuid)
	       (slot-value db 'uuid))
    (setf (slot-value db 'uuid)
	  (make-instance 'ebdb-field-uuid
			 :uuid (ebdb-make-uuid
				(slot-value db 'uuid-prefix)))))
  (cl-call-next-method db slots))

;;; Home-made auto saving for `eieio-persistent' objects.  The
;;; `ebdb-db-save' :after method deletes the auto save file, and the
;;; `ebdb-load' function checks for (and loads) any existing auto save
;;; files.

(defun ebdb-db-make-auto-save-file-name (filename)
  "Make an auto save file name from FILENAME."
  ;; What I'd really like to do, obviously, is to use the built in
  ;; `make-auto-save-file-name'.  You can't pass that function your
  ;; own filename, though, so we'll make a (highly) watered-down
  ;; version of that.
  (let ((expanded (expand-file-name filename)))
    (concat (file-name-directory expanded)
	    "#"
	    (file-name-nondirectory expanded)
	    "#")))

(cl-defmethod ebdb-db-do-auto-save ((db ebdb-db))
  (let ((auto-save-file
	 (ebdb-db-make-auto-save-file-name
	  (slot-value db 'file))))
    (eieio-persistent-save db auto-save-file)))

(defun ebdb-auto-save-databases ()
  "Auto-save all EBDB databases.

Run as a hook in the `auto-save-hook"
  (dolist (d ebdb-db-list)
    (with-slots (auto-save disabled read-only) d
      (when (and auto-save
		 (null disabled)
		 (null read-only)
		 (ebdb-db-dirty d)
		 (null (ebdb-db-unsynced d)))
	(ebdb-db-do-auto-save d)))))

(add-hook 'auto-save-hook #'ebdb-auto-save-databases)

;;; Database subclasses will have their own specialization of
;;; `ebdb-db-load', which is responsible for somehow getting the
;;; records into the DB's "records" slot.  Once that's done, they
;;; should call `cl-call-next-method' to run the methods below.

(cl-defmethod ebdb-db-unsynced ((db ebdb-db))
  "Return t if DB's persistence file has been accessed since the
last time DB was loaded.

This is the base implementation, which only checks if DB's
persistence file has been accessed.  Subclasses should combine
this check with their own check to see if their records are
somehow out of sync.

\"Unsynced\" is different from \"dirty\".  Dirty just means the
DB has unsaved changes.  Unsynced means that saving those
changes (or re-loading the database from its source) would
overwrite data somewhere."
  (let ((file-access-time
	 (nth 4
	      (file-attributes
	       (expand-file-name (slot-value db 'file))))))
    (and file-access-time
	 (time-less-p (slot-value db 'sync-time) file-access-time))))

(cl-defmethod ebdb-db-dirty ((db ebdb-db))
  "Return t if DB is marked dirty, or contains any dirty records."
  (or (slot-value db 'dirty)
      (ebdb-dirty-records (slot-value db 'records))))

(cl-defmethod ebdb-db-load ((db ebdb-db))
  "Complete the loading procedure for DB."
  ;; By this point, all the DB's records are in its record slot.
  (dolist (rec (slot-value db 'records) t)

    ;; Cycle over each loaded record.
    (condition-case err
	(progn
	  ;; Tell it about the database.
	  (object-add-to-list (ebdb-record-cache rec)
			      'database db)

	  ;; Make sure its UUID is unique.  Doesn't create new UUIDs.
	  (ebdb-check-uuid (ebdb-record-uuid rec))

	  ;; Hash the record against its UUID.  This provides some
	  ;; speedup in the later initialization process.
	  (ebdb-puthash (ebdb-record-uuid rec) rec))

      (ebdb-duplicate-uuid
       ;; There's a duplicate, decide what to do about it.  In the
       ;; following, "double" refers to the already-loaded record that
       ;; has the same uuid.  We start off assuming that we will
       ;; delete this existing "double", and replace it with the
       ;; newly-loaded record ("rec").
       (let* ((double (cadr err))
	      (delete-double t)
	      (equal (equal (clone rec :cache nil)
			    (clone double :cache nil)))
	      deleter keeper)

	 ;; If any of the double's databases are read-only, we should
	 ;; switch our assumptions about which record to delete.  This
	 ;; at least gives us a fighting chance to update a writeable
	 ;; database from a read-only one, and avoid an error.
	 (dolist (d (slot-value (ebdb-record-cache double) 'database))
	   (when (slot-value d 'read-only)
	     (setq delete-double nil)))

	 ;; If the records are the same, it doesn't matter which we
	 ;; delete, so don't change our assumption.
	 (unless equal

	   ;; But if double is newer, we still need to keep it.
	   (unless (ebdb-record-compare rec double 'timestamp)
	     (setq delete-double nil)))

	 (setq deleter (if delete-double double rec)
	       keeper (if delete-double rec double))

	 ;; Merge the records, either automatically or
	 ;; interactively, depending on the value of
	 ;; `ebdb-auto-merge-records'.
	 (unless equal
	   (setq keeper (ebdb-merge keeper deleter ebdb-auto-merge-records)))

	 (dolist (d (slot-value (ebdb-record-cache deleter) 'database))
	   ;; Use low-level functions for this so we don't set the
	   ;; database dirty.
	   (object-remove-from-list db 'records deleter)
	   (object-add-to-list db 'records keeper)
	   (object-add-to-list (ebdb-record-cache keeper)
			       'database d))
	 (ebdb-delete deleter))))))

(cl-defmethod ebdb-db-unload ((db ebdb-db))
  "Unload database DB.

This involves going through DB's records and removing each one
that doesn't belong to a different database."
  (dolist (r (slot-value db 'records))
    ;; Only disappear the record if it doesn't belong to any other
    ;; databases.
    (if (= 1 (length (slot-value (ebdb-record-cache r) 'database)))
	(ebdb-delete r db t)
      (object-remove-from-list (ebdb-record-cache r) 'database db))
    (object-remove-from-list db 'records r)))

(defun ebdb-db-reload (db)
  (ebdb-db-unload db)
  (ebdb-db-load db))

(cl-defmethod ebdb-record-compare ((left ebdb-record)
				   (right ebdb-record)
				   (_test (eql timestamp)))
  "Test if record LEFT is newer than record RIGHT."
  (let ((left-time (slot-value (slot-value left 'timestamp) 'timestamp))
	(right-time (slot-value (slot-value right 'timestamp) 'timestamp)))
    (or (equal left-time right-time)
	(time-less-p left-time right-time))))

(cl-defmethod ebdb-db-load :after ((db ebdb-db))
  (cl-pushnew db ebdb-db-list)
  (setf (slot-value db 'sync-time) (current-time))
  (run-hook-with-args 'ebdb-after-read-db-hook db))

;; This is totally goofy, it's just replacing the method overloading
;; mechanism with the hook mechanism.
(cl-defmethod ebdb-db-load :before ((db ebdb-db))
  (run-hook-with-args 'ebdb-before-read-db-hook db))

(cl-defmethod ebdb-db-editable ((db ebdb-db) &optional noerror reload)
  "Check that DB is in an editable state, and signal an error if
it isn't.  This method is called before most operations that
would alter DB.

With optional argument NOERROR, return nil instead of signalling
an error.  With optional argument RELOAD, reload DB if it is out
of sync but has no local modifications."
  (let ((err
	 (cond ((slot-value db 'read-only)
		'ebdb-readonly-db)
	       ((slot-value db 'disabled)
		'ebdb-disabled-db)
	       ((ebdb-db-unsynced db)
		(if (and (null (ebdb-db-dirty db))
			 reload)
		    (progn
		      (ebdb-db-reload db)
		      nil)
		  'ebdb-unsynced-db))
	       (t nil))))
    (or (not err)
	(if noerror
	    nil
	  (signal err (list db))))))

(cl-defmethod ebdb-db-save :before ((db ebdb-db) &optional _prompt)
  "Prepare DB to be saved."
  (when (ebdb-db-dirty db)
   (ebdb-db-editable db)))

(cl-defmethod ebdb-db-save ((db ebdb-db) &optional _prompt)
  "Save DB to its persistence file.

This method is only responsible for saving the database
definition to disk.  Database subclasses are responsible for
saving or otherwise persisting their records, and setting
their :records slot to nil before calling this method with
`cl-call-next-method'.  They can either catch errors thrown by
the persistent save, or allow them to propagate."
  (eieio-persistent-save db))

(cl-defmethod ebdb-db-save :after ((db ebdb-db) &optional _prompt)
  "After saving DB, also delete its auto-save file, if any."
  (let ((auto-save-file
	 (ebdb-db-make-auto-save-file-name
	  (slot-value db 'file))))
    (setf (slot-value db 'sync-time) (current-time))
    (when (file-exists-p auto-save-file)
      (delete-file auto-save-file))))

(cl-defmethod ebdb-db-add-record :before ((db ebdb-db) _record)
  (ebdb-db-editable db))

(cl-defmethod ebdb-db-remove-record :before ((db ebdb-db) _record)
  (ebdb-db-editable db))

(cl-defmethod ebdb-db-add-record ((db ebdb-db) record)
  "Associate RECORD with DB."
  ;; This function gets called when creating a new record, and also
  ;; when "adopting" an existing record.  In the first case, it
  ;; won't have a UUID slot.
  (unless (slot-value record 'uuid)
    (setf (slot-value record 'uuid)
	  (make-instance
	   'ebdb-field-uuid
	   :uuid (ebdb-make-uuid (slot-value db 'uuid-prefix)))))
  (object-add-to-list db 'records record)
  ;; TODO: Is there any need to sort the DB's records after insertion?
  ;; What about sorting ebdb-record-tracker?
  (object-add-to-list (ebdb-record-cache record)
		      'database db)
  record)

(cl-defmethod ebdb-db-remove-record ((db ebdb-db) record)
  "Disassociate RECORD from DB."
  (object-remove-from-list db 'records record)
  (object-remove-from-list (ebdb-record-cache record)
			   'database db)
  record)

(cl-defmethod ebdb-db-add-record-field :before ((db ebdb-db) record _slot _field)
  (ebdb-db-editable db)
  (ebdb-stamp-time record))

(cl-defmethod ebdb-db-remove-record-field :before ((db ebdb-db) record _slot _field)
  (ebdb-db-editable db)
  (ebdb-stamp-time record))

(cl-defmethod ebdb-string ((db ebdb-db))
  (format "Database: %s" (slot-value db 'file)))

(defun ebdb-db-disable (db)
  (interactive (list (ebdb-prompt-for-db)))
  (if (ebdb-db-dirty db)
      (message "Database %s has unsaved changes, you should save it first."
	       (ebdb-string db))
    (setf (slot-value db 'disabled) t)
    (setf (slot-value db 'dirty) t)
    (ebdb-db-save db)
    (ebdb-db-unload db)
    (message "Database %s is disabled." (ebdb-string db))))

(cl-defmethod ebdb-db-customize ((db ebdb-db))
  (require 'eieio-custom)
  (eieio-customize-object db))

(defun ebdb-customize-database (db)
  "Use the customization interface to edit slot values of DB."
  (interactive (list (ebdb-prompt-for-db)))
  (ebdb-db-customize db))

(cl-defmethod eieio-done-customizing ((db ebdb-db))
  (setf (slot-value db 'dirty) t)
  (cl-call-next-method))

;;; The cache class

;; This probably bears some re-thinking.  It would be nice to make it
;; behave as a "real" cache, in the sense that all the accessors are
;; accessors on the records themselves -- the records don't need to be
;; aware of the cache.  The (probably multiple) cache classes should
;; be parent classes, not slots on the record (or rather, the cache
;; slot on the record comes from the cache parent class).  We ask the
;; record for information, and the cache method intercepts the call,
;; returns the value if it has it, and if not then asks the record for
;; the value then stores it.  Ie, a real cache.  Not all the cache
;; slots would work that way, of course -- for instance. a record has
;; no way of knowing its databases except via the cache.

(defclass ebdb-cache ()
  ((name-string
    :initarg :name-string
    :type string
    :initform nil
    :documentation "The \"canonical\" name for the record, as
    displayed in the *EBDB* buffer.")
   (alt-names
    :initarg :alt-names
    :type (list-of string)
    :initform nil
    :documentation "A list of strings representing all other
    alternate names for this record.")
   (organizations
    :initarg :organizations
    :type list
    :initform nil
    :documentation
    "A list of strings representing the organizations this record
    is associated with.")
   (mail-aka
    :initarg :mail-aka
    :type list
    :initform nil)
   (mail-canon
    :initarg :mail-canon
    :type list
    :initform nil)
   (sortkey
    :initarg :sortkey
    :type string
    :initform nil)
   (database
    :initarg :database
    :type (list-of ebdb-db)
    :initform nil
    :documentation
    "The database(s) this record belongs to."))
  ;; I'm not sure if a marker slot is still going to be necessary in
  ;; this setup.
  :allow-nil-initform t)

;;; Subclasses of `ebdb-db'.

;; File-based database, keeping its records in-file.

(defclass ebdb-db-file (ebdb-db)
  nil
  :documentation "A `ebdb-db' subclass that saves records
  directly in its persistence file.")

(cl-defmethod object-print ((db ebdb-db-file) &optional strings)
  (cl-call-next-method
   db
   (append strings (format " %d records" (length (slot-value db 'records))))))

;; `ebdb-db-file' doesn't need a `ebdb-db-load' method.  Its records
;; are stored in its persistence file, directly in the :records slot,
;; so simply reading the object in with `eieio-persistent-read' does
;; all the set up we need.

(cl-defmethod ebdb-db-save ((db ebdb-db-file) &optional _prompt)
  "Mark DB and all its records as \"clean\" after saving."
  (let ((recs (ebdb-dirty-records (slot-value db 'records))))
    ;; Don't do anything if nothing's dirty.  Later we can have a
    ;; "force" argument.
    (when (or recs (slot-value db 'dirty))
      ;; These slots must be set to nil, or else we'll write ":dirty
      ;; t" slot values to file, which would be nonsensical.
      (setf (slot-value db 'dirty) nil)
      (dolist (r recs)
	(setf (slot-value r 'dirty) nil))
      (condition-case err
	  ;; This does the actual writing to file.
	  (cl-call-next-method)
	(error
	 (setf (slot-value db 'dirty) t)
	 (dolist (r recs)
	   (setf (slot-value r 'dirty) t))
	 (signal 'error err))))))

(cl-defmethod initialize-instance ((db ebdb-db-file) &optional slots)
  (let ((object-name (concat "File: " (plist-get slots :file))))
    (setq slots (plist-put slots :object-name object-name))
    (cl-call-next-method db slots)))

(cl-defmethod ebdb-db-add-record ((db ebdb-db-file) record)
  "Mark DB and RECORD as \"dirty\" until saved."
  (setf (slot-value record 'dirty) t)
  (setf (slot-value db 'dirty) t)
  (cl-call-next-method))

(cl-defmethod ebdb-db-remove-record ((db ebdb-db-file) _record)
  "Mark DB as \"dirty\" until saved."
  (setf (slot-value db 'dirty) t)
  (cl-call-next-method))

(cl-defmethod ebdb-db-add-record-field
    ((db ebdb-db-file) record _slot field)
  (setf (slot-value record 'dirty) t)
  (setf (slot-value db 'dirty) t)
  field)

(cl-defmethod ebdb-db-remove-record-field
    ((db ebdb-db-file) record _slot _field)
  (setf (slot-value record 'dirty) t)
  (setf (slot-value db 'dirty) t)
  t)

(cl-defmethod object-write ((record ebdb-record) &optional comment)
  "Don't write RECORD's cache to file."
  ;; Instead, write a clone of RECORD with the cache slot blanked out.
  (let ((clone (clone record :cache nil)))
    (cl-call-next-method clone comment)))

(defun ebdb-clear-vars ()
  "Set all internal EBDB vars to nil."
  (setq ebdb-db-list nil
	ebdb-record-tracker nil
	ebdb-seen-uuids nil)
  (clrhash ebdb-org-hashtable)
  (clrhash ebdb-hashtable))

;; Changing which database a record belongs to.

(defun ebdb-move-record (record to-db)
  "Move RECORD from its existing database to TO-DB."
  ;; It's not quite right to assume that we're *only* removing the
  ;; record from the first db in its list of dbs.
  (let ((existing (car (slot-value (ebdb-record-cache record)
				   'database))))
    (unless (equal existing to-db)
      (ebdb-db-add-record to-db record)
      (ebdb-db-remove-record existing record))))

(defun ebdb-copy-record (record to-db)
  "Copy RECORD into TO-DB."
  (ebdb-db-add-record to-db record))

;;; Utility functions

(defun ebdb-class-in-list-p (class list)
  "Check if CLASS is a member of LIST.

Both CLASS and the members of LIST should be class-name symbols.
CLASS is \"in\" list if the symbol appears directly in the list,
or if CLASS is a subclass of one of the classes in LIST.  The
function returns t in the first case, and the parent class symbol
in the second."
  (catch 'member
    (progn
      ;; First, the easy check.
      (when (memq class list)
	(throw 'member t))
      (dolist (c list nil)
	(when (child-of-class-p class c)
	  (throw 'member c))))))

(defun ebdb-dirty-dbs (&optional dbs)
  "Return all databases marked \"dirty\"."
  (let (dirty)
    (dolist (d (or dbs ebdb-db-list))
      (when (ebdb-db-dirty d)
	(push d dirty)))
    dirty))

(defun ebdb-unsynced-dbs (&optional dbs)
  "Return any databases that are out of sync with their source."
  (let (unsynced)
    (dolist (d (or dbs ebdb-db-list))
      (when (ebdb-db-unsynced d)
	(push d unsynced)))
    unsynced))

(defun ebdb-prompt-for-record (&optional records class)
  "Prompt for a single record, and return it.
If RECORDS is a list of records, offer choices from that list. If
CLASS is given, only offer choices that are an instance of that
class, or its subclasses."
  (let* ((recs (or records ebdb-record-tracker))
	 (pairs
	  (mapcar
	   (lambda (r)
	     ;; This is bad, doesn't take into account all the
	     ;; different strings that might be used to find a record.
	     (cons
	      (ebdb-string r)
	      (ebdb-record-uuid r)))
	   (if class
	       (seq-filter
		(lambda (r)
		  (object-of-class-p r class))
		recs)
	     recs)))
	 (result
	  (completing-read
	   "Choose record: "
	   pairs)))
    (ebdb-gethash (cdr (assoc-string result pairs)) 'uuid)))

(defun ebdb-prompt-for-field-type (fields)
  "Prompt the user for a field from FIELDS.

Returns a list of (\"label\" slot . field-class)."
  ;; Fields that have labels will provide those labels as a sort of
  ;; "second level" of choice. So our top-level choices should be:
  ;; mail, address, phone, notes, all subclasses of ebdb-field-user,
  ;; and then the labels in ebdb-user-label-list. With a
  ;; non-completing string assumed to be a new label for a
  ;; ebdb-user-field-simple.

  (let* (field field-list choice)
    (dolist (c fields)
      ;; Find a less-ugly way of doing this.
      (setq field (cdr c))
      (unless (or (eq field 'ebdb-field-user-simple)
		  (eq field 'ebdb-field-creation-date)
		  (eq field 'ebdb-field-timestamp))
	(push (list (ebdb-field-readable-name field) c) field-list)))
    (dolist (l ebdb-user-label-list)
      (push (list l (cons 'fields 'ebdb-field-user-simple)) field-list))
    (setq choice
	  (completing-read
	   "Choose field type: "
	   field-list))
    (or (assoc choice field-list)
	(list choice (cons 'fields 'ebdb-field-user-simple)))))

(defun ebdb-prompt-for-db (&optional db-list)
  (unless (or db-list ebdb-db-list)
    (ebdb-load))
  (let* ((collection (or db-list ebdb-db-list))
	 (db-string
	  (ebdb-read-string "Choose a database: "
			    nil
			    (mapcar
			     (lambda (d)
			       (slot-value d 'object-name))
			     collection)
			    t)))
    (object-assoc db-string 'object-name collection)))

(defun ebdb-dirty-records (&optional records)
  "Return all records with unsaved changes.

If RECORDS are given, only search those records."
  (seq-filter
   (lambda (r)
     (slot-value r 'dirty))
   (or records ebdb-record-tracker)))

;;; Getters

;; The simplest of getters/setters are defined with an :accessor tag
;; on the class slot definition itself.  Ie, `ebdb-record-user-fields'
;; and `ebdb-record-cache'.

(defun ebdb-record-cache (record)
  (slot-value record 'cache))

(defun ebdb-record-user-fields (record)
  (slot-value record 'fields))

(defun ebdb-record-user-field (record label)
  (object-assoc (if (stringp label)
		    label
		  (symbol-name label))
		'object-name (ebdb-record-user-fields record)))

(defun ebdb-record-address (record &optional label)
  (let ((addresses (slot-value record 'address)))
    (if label
	(object-assoc label 'object-name addresses)
      addresses)))

(defun ebdb-record-phone (record &optional label)
  (let ((phones (slot-value record 'phone)))
    (if label
	(object-assoc label 'object-name phones)
      phones)))

(defun ebdb-record-mail (record &optional roles label)
  (let ((mails (slot-value record 'mail)))
    (when (and roles (slot-exists-p record 'organizations))
      (dolist (r (slot-value record 'organizations))
	(when (and (slot-value r 'mail)
		   (null (slot-value r 'defunct)))
	  (push (slot-value r 'mail) mails))))
    (if label
	(object-assoc label 'object-name mails)
      mails)))


;;; Record editing

(defcustom ebdb-case-fold-search (default-value 'case-fold-search)
  "Value of `case-fold-search' used by EBDB and friends.
This variable lets the case-sensitivity of the EBDB commands
be different from standard commands like command `isearch-forward'."
  :group 'ebdb-record-edit
  :type 'boolean)

;; The following two options should be obviated by ebdb-i18n.el
;; See http://en.wikipedia.org/wiki/Postal_address
;; http://www.upu.int/en/activities/addressing/postal-addressing-systems-in-member-countstateries.html
(defcustom ebdb-address-format-list
  '(((arg) "splrc" "@%s\n@%p, @%l@, %r@\n%c@" "@%l@")
    ((aus) "slrpc" "@%s\n@%l@ %r@ %p@\n%c@" "@%l@")
    ((aut due esp che)
     "splrc" "@%s\n@%p @%l@ (%r)@\n%c@" "@%l@")
    ((can) "slrcp" "@%s\n@%l@, %r@\n%c@ %p@" "@%l@")
    ((chn) "slprc" "@%s\n@%l@\n%p@ %r@\n%c@" "@%l@") ; English format
    ; (("China") "cprls" "@%c @%p\n@%r @%l@ %s@" "@%l@") ; Chinese format
    ((ind) "slprc" "@%s\n@%l@ %p@ (%r)@\n%c@" "@%l@")
    ((usa) "slrpc" "@%s\n@%l@, %r@ %p@\n%c@" "@%l@")
    (t ebdb-edit-address-default ebdb-format-address-default "@%l@"))
  "List of address editing and formatting rules for EBDB.
Each rule is a list (IDENTIFIER EDIT FORMAT FORMAT).
The first rule for which IDENTIFIER matches an address is used for editing
and formatting the address.

IDENTIFIER may be a list of countries.
IDENTIFIER may also be a function that is called with one arg, the address
to be used.  The rule applies if the function returns non-nil.
See `ebdb-address-continental-p' for an example.
If IDENTIFIER is t, this rule always applies.  Usually, this should be
the last rule that becomes a fall-back (default).

EDIT may be a function that is called with one argument, the address.
See `ebdb-edit-address-default' for an example.

EDIT may also be an editting format string.  It is a string containing
the five letters s, c, p, S, and C that specify the order for editing
the five elements of an address:

s  streets
l  locality
p  postcode
r  region
c  country

The first FORMAT of each rule is used for multi-line layout, the second FORMAT
is used for one-line layout.

FORMAT may be a function that is called with one argument, the address.
See `ebdb-format-address-default' for an example.

FORMAT may also be a format string.  It consists of formatting elements
separated by a delimiter defined via the first (and last) character of FORMAT.
Each formatting element may contain one of the following format specifiers:

%s  streets (used repeatedly for each street part)
%l  locality
%p  postcode
%r  region
%c  country

A formatting element will be applied only if the corresponding part
of the address is a non-empty string.

See also `ebdb-print-address-format-list'."
  :group 'ebdb-record-edit
  :type '(repeat (list (choice (const :tag "Default" t)
                               (function :tag "Function")
                               (repeat (string)))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function"))
                       (choice (string)
                               (function :tag "Function")))))

(defcustom ebdb-continental-postcode-regexp
  "^\\s *[A-Z][A-Z]?\\s *-\\s *[0-9][0-9][0-9]"
  "Regexp matching continental postcodes.
Used by address format identifier `ebdb-address-continental-p'.
The regexp should match postcodes of the form CH-8052, NL-2300RA,
and SE-132 54."
  :group 'ebdb-record-edit
  :type 'regexp)

(defcustom ebdb-default-separator '("[,;]" ", ")
  "The default field separator.  It is a list (SPLIT-RE JOIN).
This is used for fields which do not have an entry in `ebdb-separator-alist'."
  :group 'ebdb-record-edit
  :type '(list regexp string))

(defcustom ebdb-separator-alist
  '((record "\n\n" "\n\n") ; used by `ebdb-copy-fields-as-kill'
    (name-first-last "[ ,;]" " ") (name-last-first "[ ,;]" ", ")
    (name-field ":\n" ":\n") ; used by `ebdb-copy-fields-as-kill'
    (phone "[,;]" ", ") (address ";\n" ";\n") ; ditto
    (organization "[,;]" ", ") (affix "[,;]"  ", ") (aka "[,;]" ", ")
    (mail "[,;]" ", ") (mail-alias "[,;]" ", ") (vm-folder "[,;]" ", ")
    (birthday "\n" "\n") (wedding "\n" "\n") (anniversary "\n" "\n")
    (notes "\n" "\n"))
  "Alist of field separators.
Each element is of the form (FIELD SPLIT-RE JOIN).
For fields lacking an entry here `ebdb-default-separator' is used instead."
  :group 'ebdb-record-edit
  :type '(repeat (list symbol regexp string)))

(defcustom ebdb-image-path nil
  "List of directories to search for `ebdb-image'."
  :group 'ebdb-record-edit
  :type '(repeat (directory)))

(defcustom ebdb-image-suffixes '(".png" ".jpg" ".gif" ".xpm")
  "List of file name suffixes searched for `ebdb-image'."
  :group 'ebdb-record-edit
  :type '(repeat (string :tag "File suffix")))

(defcustom ebdb-read-name-articulate nil
  "Specify how to read record names.

If nil, read full names as single strings, and parse them
accordingly.  If t, the user will be prompted separately for each
field of the name.

If this option is nil, and the user enters a single string, the
resulting name field will be an instance of
`ebdb-field-name-simple'.  Even if this option is t, the user can
still trigger the creation of a simple name field by entering a
single string for the surname, and nothing else."
  :group 'ebdb-record-edit
  :type 'boolean)

(defcustom ebdb-lastname-prefixes
 '("von" "de" "di")
  "List of lastname prefixes recognized in name fields.
Used to enhance dividing name strings into firstname and lastname parts.
Case is ignored."
  :group 'ebdb-record-edit
  :type '(repeat string))

(defcustom ebdb-lastname-re
  (concat "[- \t]*\\(\\(?:\\<"
          (regexp-opt ebdb-lastname-prefixes)
          ;; multiple last names concatenated by `-'
          "\\>[- \t]+\\)?\\(?:\\w+[ \t]*-[ \t]*\\)*\\w+\\)\\'")
  "Regexp matching the last name of a full name.
Its first parenthetical subexpression becomes the last name."
  :group 'ebdb-record-edit
  :type 'regexp)

(defcustom ebdb-lastname-suffixes
 '("Jr" "Sr" "II" "III")
  "List of lastname suffixes recognized in name fields.
Used to dividing name strings into firstname and lastname parts.
All suffixes are complemented by optional `.'.  Case is ignored."
  :group 'ebdb-record-edit
  :type '(repeat string))

(defcustom ebdb-lastname-suffix-re
  (concat "[-,. \t/\\]+\\("
          (regexp-opt ebdb-lastname-suffixes)
          ;; suffices are complemented by optional `.'.
          "\\.?\\)\\W*\\'")
  "Regexp matching the suffix of a last name.
Its first parenthetical subexpression becomes the suffix."
  :group 'ebdb-record-edit
  :type 'regexp)

(defcustom ebdb-default-domain nil
  "Default domain to append when reading a new mail address.
If a mail address does not contain `[@%!]', append @`ebdb-default-domain' to it.

The address is not altered if `ebdb-default-domain' is nil
or if a prefix argument is given to the command `ebdb-insert-field'."
  :group 'ebdb-record-edit
  :type '(choice (const :tag "none" nil)
                 (string :tag "Default Domain")))

(defcustom ebdb-allow-duplicates nil
  "When non-nil EBDB allows records with duplicate names and email addresses.
In rare cases, this may lead to confusion with EBDB's MUA interface."
  :group 'ebdb-record-edit
  :type 'boolean)

(defcustom ebdb-default-label-list '("home" "work" "other")
  "Default list of labels for Address and Phone fields."
  :group 'ebdb-record-edit
  :type '(repeat string))

(defcustom ebdb-address-label-list ebdb-default-label-list
  "List of labels for Address field."
  :group 'ebdb-record-edit
  :type '(repeat string))

(defcustom ebdb-phone-label-list '("home" "work" "cell" "fax" "other")
  "List of labels for Phone field."
  :group 'ebdb-record-edit
  :type '(repeat string))

(defcustom ebdb-default-country "Emacs";; what do you mean, it's not a country?
  "Default country to use if none is specified."
  :group 'ebdb-record-edit
  :type '(choice (const :tag "None" nil)
                 (string :tag "Default Country")))

(defcustom ebdb-check-postcode t
  "If non-nil, require legal postcodes when entering an address.
The format of legal postcodes is determined by the variable
`ebdb-legal-postcodes'."
  :group 'ebdb-record-edit
  :type 'boolean)

(defcustom ebdb-legal-postcodes
  '(;; empty string
    "^$"
    ;; Matches 1 to 6 digits.
    "^[ \t\n]*[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[ \t\n]*$"
    ;; Matches 5 digits and 3 or 4 digits.
    "^[ \t\n]*\\([0-9][0-9][0-9][0-9][0-9]\\)[ \t\n]*-?[ \t\n]*\\([0-9][0-9][0-9][0-9]?\\)[ \t\n]*$"
    ;; Match postcodes for Canada, UK, etc. (result is ("LL47" "U4B")).
    "^[ \t\n]*\\([A-Za-z0-9]+\\)[ \t\n]+\\([A-Za-z0-9]+\\)[ \t\n]*$"
    ;; Match postcodes for continental Europe.  Examples "CH-8057"
    ;; or "F - 83320" (result is ("CH" "8057") or ("F" "83320")).
    ;; Support for "NL-2300RA" added at request from Carsten Dominik
    ;; <dominik@astro.uva.nl>
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+ ?[A-Z]*\\)[ \t\n]*$"
    ;; Match postcodes from Sweden where the five digits are grouped 3+2
    ;; at the request from Mats Lofdahl <MLofdahl@solar.stanford.edu>.
    ;; (result is ("SE" (133 36)))
    "^[ \t\n]*\\([A-Z]+\\)[ \t\n]*-?[ \t\n]*\\([0-9]+\\)[ \t\n]+\\([0-9]+\\)[ \t\n]*$")
  "List of regexps that match legal postcodes.
Whether this is used at all depends on the variable `ebdb-check-postcode'."
  :group 'ebdb-record-edit
  :type '(repeat regexp))

(defcustom ebdb-default-user-field 'notes
  "Default field when editing EBDB records."
  :group 'ebdb-record-edit
  :type '(symbol :tag "Field"))



(defcustom ebdb-mail-name-format 'first-last
  "Format for names when sending mail.
If first-last format names as \"Firstname Lastname\".
If last-first format names as \"Lastname, Firstname\".
If `ebdb-mail-name' returns the full name as a single string, this takes
precedence over `ebdb-mail-name-format'.  Likewise, if the mail address itself
includes a name, this is not reformatted."
  :group 'ebdb-sendmail
  :type '(choice (const :tag "Firstname Lastname" first-last)
                 (const :tag "Lastname, Firstname" last-first)))

(defcustom ebdb-mail-name 'mail-name
  "Xfield holding the full name for a record when sending mail.
This may also be a function taking one argument, a record.
If it returns the full mail name as a single string, this is used \"as is\".
If it returns a cons pair (FIRST . LAST) with the first and last name
for this record, these are formatted obeying `ebdb-mail-name-format'."
  :group 'ebdb-sendmail
  :type '(choice (symbol :tag "xfield")
                 (function :tag "mail name function")))

(defcustom ebdb-mail-alias-field 'mail-alias
  "Xfield holding the mail alias for a record.
Used by `ebdb-mail-aliases'.  See also `ebdb-mail-alias'."
  :group 'ebdb-sendmail
  :type 'symbol)

(defcustom ebdb-mail-alias 'first
  "Defines which mail aliases are generated for a EBDB record.
first: Generate one alias \"<alias>\" that expands to the first mail address
       of a record.
star:  Generate a second alias \"<alias>*\" that expands to all mail addresses
       of a record.
all:   Generate the aliases \"<alias>\" and \"<alias>*\" (as for 'star)
       and aliases \"<alias>n\" for each mail address, where n is the position
       of the mail address of a record."
  :group 'ebdb-sendmail
  :type '(choice (symbol :tag "Only first" first)
                 (symbol :tag "<alias>* for all mails" star)
                 (symbol :tag "All aliases" all)))

(defcustom ebdb-mail-avoid-redundancy nil
  "Mail address to use for EBDB records when sending mail.
If non-nil do not use full name in mail address when same as mail.
If value is mail-only never use full name."
  :group 'ebdb-sendmail
  :type '(choice (const :tag "Allow redundancy" nil)
                 (const :tag "Never use full name" mail-only)
                 (const :tag "Avoid redundancy" t)))

(defcustom ebdb-complete-mail t
  "If t MUA insinuation provides key binding for command `ebdb-complete-mail'."
  :group 'ebdb-sendmail
  :type 'boolean)

(defcustom ebdb-completion-list t
  "Controls the behaviour of `ebdb-complete-mail'.
If a list of symbols, it specifies which fields to complete.  Symbols include
  name (= record's display name)
  alt-names (= any other names the record has)
  organization
  mail (= all email addresses of each record)
  primary (= first email address of each record)
If t, completion is done for all of the above.
If nil, no completion is offered."
  ;; These symbols match the fields for which EBDB provides entries in
  ;; `ebdb-hash-table'.
  :group 'ebdb-sendmail
  :type '(choice (const :tag "No Completion" nil)
                 (const :tag "Complete across all fields" t)
                 (repeat :tag "Field"
                         (choice (const name)
                                 (const alt-names)
                                 (const organization)
                                 (const primary)
                                 (const mail)))))

(defcustom ebdb-complete-mail-allow-cycling nil
  "If non-nil cycle mail addresses when calling `ebdb-complete-mail'."
  :group 'ebdb-sendmail
  :type 'boolean)

(defcustom ebdb-complete-mail-hook nil
  "List of functions called after a sucessful completion."
  :group 'ebdb-sendmail
  :type 'hook)

(defcustom ebdb-mail-abbrev-expand-hook nil
  ;; Replacement for function `mail-abbrev-expand-hook'.
  "Function (not hook) run each time an alias is expanded.
The function is called with two args the alias and the list
of corresponding mail addresses."
  :group 'ebdb-sendmail
  :type 'function)

(defcustom ebdb-completion-display-record t
  "If non-nil `ebdb-complete-mail' displays the EBDB record after completion."
  :group 'ebdb-sendmail
  :type '(choice (const :tag "Update the EBDB buffer" t)
                 (const :tag "Do not update the EBDB buffer" nil)))


;;;Dialing
(defcustom ebdb-dial-local-prefix-alist
  '(((if (integerp ebdb-default-area-code)
         (format "(%03d)" ebdb-default-area-code)
       (or ebdb-default-area-code ""))
     . ""))
  "Mapping to remove local prefixes from numbers.
If this is non-nil, it should be an alist of
\(PREFIX . REPLACEMENT) elements. The first part of a phone number
matching the regexp returned by evaluating PREFIX will be replaced by
the corresponding REPLACEMENT when dialing."
  :group 'ebdb-utilities-dialing
  :type 'sexp)

(defcustom ebdb-dial-local-prefix nil
  "Local prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making local calls (for example, if your phone system
requires you to dial 9 before making outside calls.) In EBDB's
opinion, you're dialing a local number if it starts with a 0 after
processing `ebdb-dial-local-prefix-alist'."
  :group 'ebdb-utilities-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "9")))

(defcustom ebdb-dial-long-distance-prefix nil
  "Long distance prefix digits.
If this is non-nil, it should be a string of digits which your phone
system requires before making a long distance call (one not in your local
area code).  For example, in some areas you must dial 1 before an area
code. Note that this is used to replace the + sign in phone numbers
when dialling (international dialing prefix.)"
  :group 'ebdb-utilities-dialing
  :type '(choice (const :tag "No digits required" nil)
                 (string :tag "Dial this first" "1")))

(defcustom ebdb-dial-function nil
  "If non-nil this should be a function used for dialing phone numbers.
This function is used by `ebdb-dial-number'.  It requires one
argument which is a string for the number that is dialed.
If nil then `ebdb-dial-number' uses the tel URI syntax passed to `browse-url'
to make the call."
  :group 'ebdb-utilities-dialing
  :type 'function)



;;; Helper functions

(defun ebdb-warn (&rest args)
  "Display a message at the bottom of the screen.
ARGS are passed to `message'."
  (ding t)
  (apply 'message args))

(defun ebdb-string-trim (string &optional null)
  "Remove leading and trailing whitespace and all properties from STRING.
If STRING is nil return an empty string unless NULL is non-nil."
  (if (null string)
      (unless null "")
    (setq string (substring-no-properties (string-trim string)))
    (unless (and null (string-empty-p string))
      string)))

(defsubst ebdb-string= (str1 str2)
  "Return t if strings STR1 and STR2 are equal, ignoring case."
  (and (stringp str1) (stringp str2)
       (eq t (compare-strings str1 0 nil str2 0 nil t))))

(defun ebdb-split (separator string)
  "Split STRING into list of substrings bounded by matches for SEPARATORS.
SEPARATOR may be a regexp.  SEPARATOR may also be a symbol
\(a field name).  Then look up the value in `ebdb-separator-alist'
or use `ebdb-default-separator'.
Whitespace around SEPARATOR is ignored unless SEPARATOR matches
the string \" \\t\\n\".
Almost the inverse function of `ebdb-concat'."
  (if (symbolp separator)
      (setq separator (car (or (cdr (assq separator ebdb-separator-alist))
                               ebdb-default-separator))))
  (unless (string-match separator " \t\n")
    (setq separator (concat "[ \t\n]*" separator "[ \t\n]*")))
  ;; `split-string' applied to an empty STRING gives nil.
  (split-string (ebdb-string-trim string) separator t))

(defun ebdb-concat (separator &rest strings)
  "Concatenate STRINGS to a string sticking in SEPARATOR.
STRINGS may be strings or lists of strings.  Empty strings are ignored.
SEPARATOR may be a string.
SEPARATOR may also be a symbol (a field name).  Then look up the value
of SEPARATOR in `ebdb-separator-alist' or use `ebdb-default-separator'.
The inverse function of `ebdb-split'."
  (if (symbolp separator)
      (setq separator (nth 1 (or (cdr (assq separator ebdb-separator-alist))
                                 ebdb-default-separator))))
  (mapconcat 'identity
             (delete "" (apply 'append (mapcar (lambda (x) (if (stringp x)
                                                               (list x) x))
                                               strings))) separator))

(defun ebdb-list-strings (list)
  "Remove all elements from LIST which are not non-empty strings."
  (let (new-list)
    (dolist (elt list)
      (if (and (stringp elt) (not (string= "" elt)))
          (push elt new-list)))
    (nreverse new-list)))

(defun ebdb-read-string (prompt &optional init collection require-match)
  "Read a string, trimming whitespace and text properties.
PROMPT is a string to prompt with.
INIT appears as initial input which is useful for editing existing records.
COLLECTION and REQUIRE-MATCH have the same meaning as in `completing-read'."
  (ebdb-string-trim
   (if collection
       ;; Hack: In `minibuffer-local-completion-map' remove
       ;; the binding of SPC to `minibuffer-complete-word'
       ;; and of ? to `minibuffer-completion-help'.
       (minibuffer-with-setup-hook
           (lambda ()
             (use-local-map
              (let ((map (make-sparse-keymap)))
                (set-keymap-parent map (current-local-map))
                (define-key map " " nil)
                (define-key map "?" nil)
                map)))
         (completing-read prompt collection nil require-match init))
     (let ((string (read-string prompt init)))
       (if (string= "" string)
	   (signal 'ebdb-empty (list prompt))
	 string)))))

(defun ebdb-add-to-list (list-var element)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet and non-nil.
The test for presence of ELEMENT is done with `equal'.
The return value is the new value of LIST-VAR."
  ;; Unlike `add-to-list' this ignores ELEMENT if it is nil.
  ;; TO DO: turn this into a faster macro so that we can abandon calls
  ;; of add-to-list.
  (if (or (not element)
          (member element (symbol-value list-var)))
      (symbol-value list-var)
    (set list-var (cons element (symbol-value list-var)))))

(defsubst ebdb-add-job (spec record string)
  "Internal function: Evaluate SPEC for RECORD and STRING.
If SPEC is a function call it with args RECORD and STRING.  Return value.
If SPEC is a regexp, return 'query unless SPEC matches STRING.
Otherwise return SPEC.
Used with variable `ebdb-add-name' and friends."
  (cond ((functionp spec)
         (funcall spec record string))
        ((stringp spec)
         (unless (string-match spec string) 'query)) ; be least aggressive
        (spec)))

(defsubst ebdb-eval-spec (spec prompt)
  "Internal function: Evaluate SPEC using PROMPT.
Return t if either SPEC equals t, or SPEC equals 'query and `ebdb-silent'
is non-nil or `y-or-no-p' returns t using PROMPT.
Used with return values of `ebdb-add-job'."
  (or (eq spec t)
      (and (eq spec 'query)
           (or ebdb-silent (y-or-n-p prompt)))))

(defun ebdb-clean-address-components (components)
  "Clean mail address COMPONENTS.
COMPONENTS is a list (FULL-NAME CANONICAL-ADDRESS) as returned
by `mail-extract-address-components'.
Pass FULL-NAME through `ebdb-message-clean-name-function'
and CANONICAL-ADDRESS through `ebdb-canonicalize-mail-function'."
  (list (if (car components)
            (if ebdb-message-clean-name-function
                (funcall ebdb-message-clean-name-function (car components))
              (car components)))
        (if (cadr components)
            (if ebdb-canonicalize-mail-function
                (funcall ebdb-canonicalize-mail-function (cadr components))
              ;; Minimalistic clean-up
              (ebdb-string-trim (cadr components))))))

(defun ebdb-extract-address-components (address &optional all)
  "Given an RFC-822 address ADDRESS, extract full name and canonical address.
This function behaves like `mail-extract-address-components', but it passes
its return value through `ebdb-clean-address-components'.
See also `ebdb-decompose-ebdb-address'."
  (if all
      (mapcar 'ebdb-clean-address-components
              (mail-extract-address-components address t))
    (ebdb-clean-address-components (mail-extract-address-components address))))

;; Inspired by `gnus-extract-address-components' from gnus-utils.
(defun ebdb-decompose-ebdb-address (mail)
  "Given an RFC-822 address MAIL, extract full name and canonical address.
In general, this function behaves like the more sophisticated function
`mail-extract-address-components'.  Yet for an address `<Joe_Smith@foo.com>'
lacking a real name the latter function returns the name \"Joe Smith\".
This is useful when analyzing the headers of email messages we receive
from the outside world.  Yet when analyzing the mail addresses stored
in EBDB, this pollutes the mail-aka space.  So we define here
an intentionally much simpler function for decomposing the names
and canonical addresses in the mail field of EBDB records."
  (let (name address)
    ;; First get rid of any "mailto:" in the string.
    (setq mail (replace-regexp-in-string "mailto:" "" mail))
    ;; Then find the address - the thing with the @ in it.
    (cond (;; Check `<foo@bar>' first in order to handle the quite common
	   ;; form `"abc@xyz" <foo@bar>' (i.e. `@' as part of a comment)
	   ;; correctly.
	   (string-match "<\\([^@ \t<>]+[!@][^@ \t<>]+\\)>" mail)
	   (setq address (match-string 1 mail)))
	  ((string-match "\\b[^@ \t<>]+[!@][^@ \t<>]+\\b" mail)
	   (setq address (match-string 0 mail))))
    ;; Then check whether the `name <address>' format is used.
    (and address
	 ;; Linear white space is not required.
	 (string-match (concat "[ \t]*<?" (regexp-quote address) ">?") mail)
	 (setq name (substring mail 0 (match-beginning 0)))
         ;; Strip any quotes mail the name.
         (string-match "^\".*\"$" name)
         (setq name (substring name 1 (1- (match-end 0)))))
    ;; If not, then check whether the `address (name)' format is used.
    (or name
	(and (string-match "(\\([^)]+\\))" mail)
	     (setq name (match-string 1 mail))))
    (list (if (equal name "") nil name) (or address mail))))

;;; Massage of mail addresses

(defcustom ebdb-canonical-hosts
  ;; Example
  (regexp-opt '("cs.cmu.edu" "ri.cmu.edu"))
  "Regexp matching the canonical part of the domain part of a mail address.
If the domain part of a mail address matches this regexp, the domain
is replaced by the substring that actually matched this address.

Used by  `ebdb-canonicalize-mail-1'.  See also `ebdb-ignore-redundant-mails'."
  :group 'ebdb-mua
  :type '(regexp :tag "Regexp matching sites"))

(defun ebdb-canonicalize-mail-1 (address)
  "Example of `ebdb-canonicalize-mail-function'.
However, this function is too specific to be useful for the general user.
Take it as a source of inspiration for what can be done."
  (setq address (ebdb-string-trim address))
  (cond
   ;; Rewrite mail-drop hosts.
   ;; RW: The following is now also handled by `ebdb-ignore-redundant-mails'
   ((string-match
     (concat "\\`\\([^@%!]+@\\).*\\.\\(" ebdb-canonical-hosts "\\)\\'")
     address)
    (concat (match-string 1 address) (match-string 2 address)))
   ;;
   ;; Here at Lucid, our workstation names sometimes get into our mail
   ;; addresses in the form "jwz%thalidomide@lucid.com" (instead of simply
   ;; "jwz@lucid.com").  This removes the workstation name.
   ((string-match "\\`\\([^@%!]+\\)%[^@%!.]+@\\(lucid\\.com\\)\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another way that our local mailer is misconfigured: sometimes addresses
   ;; which should look like "user@some.outside.host" end up looking like
   ;; "user%some.outside.host" or even "user%some.outside.host@lucid.com"
   ;; instead.  This rule rewrites it into the original form.
   ((string-match "\\`\\([^@%]+\\)%\\([^@%!]+\\)\\(@lucid\\.com\\)?\\'" address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user@foobar.com".
   ;; That's totally redundant, so this rewrites it as "user@foobar.com".
   ((string-match "\\`\\([^@%!]+\\)!\\([^@%!]+[@%]\\1\\)\\'" address)
    (match-string 2 address))
   ;;
   ;; Sometimes I see addresses like "foobar.com!user".  Turn it around.
   ((string-match "\\`\\([^@%!.]+\\.[^@%!]+\\)!\\([^@%]+\\)\\'" address)
    (concat (match-string 2 address) "@" (match-string 1 address)))
   ;;
   ;; The mailer at hplb.hpl.hp.com tends to puke all over addresses which
   ;; pass through mailing lists which are maintained there: it turns normal
   ;; addresses like "user@foo.com" into "user%foo.com@hplb.hpl.hp.com".
   ;; This reverses it.  (I actually could have combined this rule with
   ;; the similar lucid.com rule above, but then the regexp would have been
   ;; more than 80 characters long...)
   ((string-match "\\`\\([^@!]+\\)%\\([^@%!]+\\)@hplb\\.hpl\\.hp\\.com\\'"
          address)
    (concat (match-string 1 address) "@" (match-string 2 address)))
   ;;
   ;; Another local mail-configuration botch: sometimes mail shows up
   ;; with addresses like "user@workstation", where "workstation" is a
   ;; local machine name.  That should really be "user" or "user@netscape.com".
   ;; (I'm told this one is due to a bug in SunOS 4.1.1 sendmail.)
   ((string-match "\\`\\([^@%!]+\\)[@%][^@%!.]+\\'" address)
    (match-string 1 address))
   ;;
   ;; Sometimes I see addresses like "foo%somewhere%uunet.uu.net@somewhere.else".
   ;; This is silly, because I know that I can send mail to uunet directly.
   ((string-match ".%uunet\\.uu\\.net@[^@%!]+\\'" address)
    (concat (substring address 0 (+ (match-beginning 0) 1)) "@UUNET.UU.NET"))
   ;;
   ;; Otherwise, leave it as it is.
   (t address)))

(defun ebdb-message-clean-name-default (name)
  "Default function for `ebdb-message-clean-name-function'.
This strips garbage from the user full NAME string."
  ;; Remove leading non-alpha chars
  (if (string-match "\\`[^[:alpha:]]+" name)
      (setq name (substring name (match-end 0))))

  (if (string-match "^\\([^@]+\\)@" name)
      ;; The name is really a mail address and we use the part preceeding "@".
      ;; Everything following "@" is ignored.
      (setq name (match-string 1 name)))

  ;; Replace "firstname.surname" by "firstname surname".
  ;; Do not replace ". " with " " because that could be an initial.
  (setq name (replace-regexp-in-string "\\.\\([^ ]\\)" " \\1" name))

  ;; Replace tabs, spaces, and underscores with a single space.
  (setq name (replace-regexp-in-string "[ \t\n_]+" " " name))

  ;; Remove trailing comments separated by "(" or " [-#]"
  ;; This does not work all the time because some of our friends in
  ;; northern europe have brackets in their names...
  (if (string-match "[^ \t]\\([ \t]*\\((\\| [-#]\\)\\)" name)
      (setq name (substring name 0 (match-beginning 1))))

  ;; Remove phone extensions (like "x1234" and "ext. 1234")
  (let ((case-fold-search t))
    (setq name (replace-regexp-in-string
                "\\W+\\(x\\|ext\\.?\\)\\W*[-0-9]+" "" name)))

  ;; Remove trailing non-alpha chars
  (if (string-match "[^[:alpha:]]+\\'" name)
      (setq name (substring name 0 (match-beginning 0))))

  ;; Remove text properties
  (substring-no-properties name))

(defsubst ebdb-record-mail-aka (record)
  "Record cache function: Return mail-aka for RECORD."
  (slot-value (ebdb-record-cache record) 'mail-aka))

(defsubst ebdb-record-mail-canon (record)
  "Record cache function: Return mail-canon for RECORD."
  (slot-value (ebdb-record-cache record) 'mail-canon))

;; `ebdb-hashtable' associates with each KEY a list of matching records.
;; KEY includes fl-name, lf-name, organizations, AKAs and email addresses.
;; When loading the database the hash table is initialized by calling
;; `ebdb-hash-record' for each record.  This function is also called
;; when new records are added to the database.

(defun ebdb-puthash (key record)
  "Associate RECORD with KEY in `ebdb-hashtable'.
KEY must be a string or nil.  Empty strings and nil are ignored."
  (if (and key (not (string-empty-p key))) ; do not hash empty strings
      (let* ((key (downcase key))
	     (records (gethash key ebdb-hashtable)))
	(puthash key (if records (cl-pushnew record records)
                       (list record))
		 ebdb-hashtable))))

(defun ebdb-gethash (key &optional predicate)
  "Return list of records associated with KEY in
`ebdb-hashtable'.  KEY must be a string or nil.  Empty strings
and nil are ignored.  PREDICATE may take the same values as
`ebdb-completion-list'.  If predicate is the single symbol uuid,
this function returns a single record."
  (when (and key (not (string-empty-p key)))
    (let* ((key (downcase key))
           (all-records (gethash key ebdb-hashtable))
           records)
      (if (or (not predicate) (eq t predicate))
          all-records
        (if (eql predicate 'uuid)
	    ;; We could conceivably do a bit of sanity checking here.
	    ;; All-records should only be a list of one.
	    (car all-records)
	  (dolist (record all-records records)
	    (if (catch 'ebdb-hash-ok
		  (ebdb-hash-p key record predicate))
		(push record records))))))))

(defun ebdb-hash-p (key record predicate)
  "Throw `ebdb-hash-ok' non-nil if KEY matches RECORD acording to PREDICATE.
PREDICATE may take the same values as the elements of `ebdb-completion-list'."
  (if (and (memq 'fl-name predicate)
           (ebdb-string= key (or (ebdb-record-name record) "")))
      (throw 'ebdb-hash-ok 'fl-name))
  ;; (if (and (memq 'lf-name predicate)
  ;;          (ebdb-string= key (or (ebdb-record-name-lf record) "")))
  ;;     (throw 'ebdb-hash-ok 'lf-name))
  (if (memq 'organization predicate)
      (mapc (lambda (organization) (if (ebdb-string= key organization)
                                       (throw 'ebdb-hash-ok 'organization)))
            (ebdb-record-organization record)))
  (if (memq 'aka predicate)
      (mapc (lambda (aka) (if (ebdb-string= key (ebdb-string aka))
                              (throw 'ebdb-hash-ok 'aka)))
            (slot-value record 'aka)))
  (if (and (memq 'primary predicate)
           (ebdb-string= key (car (ebdb-record-mail-canon record))))
      (throw 'ebdb-hash-ok 'primary))
  (if (memq 'mail predicate)
      (mapc (lambda (mail) (if (ebdb-string= key mail)
                               (throw 'ebdb-hash-ok 'mail)))
            (ebdb-record-mail-canon record)))
  nil)

(defun ebdb-remhash (key record)
  "Remove RECORD from list of records associated with KEY.
KEY must be a string or nil.  Empty strings and nil are ignored."
  (if (and key (not (string-empty-p key)))
      (let* ((key (downcase key))
	     (records (gethash key ebdb-hashtable)))
        (when records
          (setq records (delq record records))
          (if records
              (puthash key records ebdb-hashtable)
            (remhash key ebdb-hashtable))))))

(defun ebdb-hash-update (record old new)
  "Update hash for RECORD.  Remove OLD, insert NEW.
Both OLD and NEW are lists of values."
  (dolist (elt old)
    (ebdb-remhash elt record))
  (dolist (elt new)
    (ebdb-puthash elt record)))

(defun ebdb-check-name (first last &optional record)
  "Check whether the name FIRST LAST is a valid name.
This throws an error if the name is already used by another record
and `ebdb-allow-duplicates' is nil.  If RECORD is non-nil, FIRST and LAST
may correspond to RECORD without raising an error."
  ;; Are there more useful checks for names beyond checking for duplicates?
  (unless ebdb-allow-duplicates
    (let* ((name (ebdb-concat 'name-first-last first last))
           (records (ebdb-gethash name '(fl-name lf-name aka))))
      (if (or (and (not record) records)
              (remq record records))
          (error "%s is already in EBDB" name)))))

(defun ebdb-record-sortkey (record)
  "Record cache function: Return the sortkey for RECORD.
Set and store it if necessary."
  (or (slot-value (ebdb-record-cache record) 'sortkey)
      (ebdb-record-set-sortkey record)))

(cl-defmethod ebdb-record-set-sortkey ((record ebdb-record-person))
  "Record cache function: Set and return RECORD's sortkey."
  (setf
   (slot-value (ebdb-record-cache record) 'sortkey)
   (downcase (ebdb-name-lf (slot-value record 'name)))))

(cl-defmethod ebdb-record-set-sortkey ((record ebdb-record-organization))
  (setf
   (slot-value (ebdb-record-cache record) 'sortkey)
   (downcase (ebdb-string (slot-value record 'name)))))

;; The values of xfields are normally strings.  The following function
;; comes handy if we want to treat these values as symbols.
;; (defun ebdb-record-xfield-intern (record label)
;;   "For RECORD return interned value of xfield LABEL.
;; Return nil if xfield LABEL does not exist."
;;   (let ((value (ebdb-record-xfield record label)))
;;     ;; If VALUE is not a string, return whatever it is.
;;     (if (stringp value) (intern value) value)))

;; (defun ebdb-record-xfield-string (record label)
;;   "For RECORD return value of xfield LABEL as string.
;; Return nil if xfield LABEL does not exist."
;;   (let ((value (ebdb-record-xfield record label)))
;;     (if (string-or-null-p value)
;;         value
;;       (let ((print-escape-newlines t))
;;         (prin1-to-string value)))))

;; (defsubst ebdb-record-xfield-split (record label)
;;   "For RECORD return value of xfield LABEL split as a list.
;; Splitting is based on `ebdb-separator-alist'."
;;   (let ((val (ebdb-record-xfield record label)))
;;     (cond ((stringp val) (ebdb-split label val))
;;           (val (error "Cannot split `%s'" val)))))

(cl-defgeneric ebdb-record-field (record field)
  "For RECORD return the value of FIELD.

FIELD may be a slot-name symbol, in which case the value of that
slot, if any, is returned.  It can be a string, in which case it
is interpreted as a label for one of RECORD's user fields.  It
can also be the symbol name of a user-field class, in which case
all the record's instances of that class are returned.  It can
also be one of the special symbols below.

 firstname     Return the first name of RECORD
 lastname      Return the last name of RECORD
 name-lf       Return the full name of RECORD (last name first)
 affix         Return the list of affixes
 aka-all       Return the list of AKAs plus mail-akas.
 mail-aka      Return the list of name parts in mail addresses
 mail-canon    Return the list of canonical mail addresses.")

(cl-defmethod ebdb-record-field ((record ebdb-record)
				 (field symbol))
  (cond ((eq field 'firstname) (ebdb-record-firstname record))
	((eq field 'lastname) (ebdb-record-lastname record))
	((eq field 'affix)    (slot-value (slot-value record 'name) 'affix))
	((eq field 'mail-canon) (ebdb-record-mail-canon record)) ; derived (cached) field
	((eq field 'mail-aka) (ebdb-record-mail-aka record)) ; derived (cached) field
	((eq field 'aka-all)  (append (ebdb-record-aka record) ; derived field
				      (ebdb-record-mail-aka record)))
	;; It might be a class symbol.
	((class-p field)
	 (seq-filter
	  (lambda (f)
	    (object-of-class-p f field))
	  (ebdb-record-user-fields record)))
	;; Otherwise assume it is a valid slot name.
	(t
	 (when (and (slot-exists-p record field)
		    (slot-boundp record field))
	   (slot-value record field)))))

(cl-defmethod ebdb-record-field ((record ebdb-record)
				 (field string))
  (let ((user-fields (ebdb-record-user-fields record)))
    (catch 'found
      (dolist (f user-fields)
	(when (and (slot-exists-p f 'object-name)
		   (string= field (slot-value f 'object-name)))
	  (throw 'found f))))))

(defun ebdb-merge-concat (string1 string2 &optional separator)
  "Return the concatenation of STRING1 and STRING2.
SEPARATOR defaults to \"\\n\"."
  (concat string1 (or separator "\n") string2))

(defun ebdb-merge-concat-remove-duplicates (string1 string2)
  "Concatenate STRING1 and STRING2, but remove duplicate lines."
  (let ((lines (split-string string1 "\n")))
    (dolist (line (split-string string2 "\n"))
      (cl-pushnew line lines))
    (ebdb-concat "\n" lines)))

(defun ebdb-merge-string-least (string1 string2)
  "Return the string out of STRING1 and STRING2 that is `string-lessp'."
  (if (string-lessp string1 string2)
      string1
    string2))

(defun ebdb-merge-string-most (string1 string2)
  "Return the string out of STRING1 and STRING2 that is not `string-lessp'."
  (if (string-lessp string1 string2)
      string2
    string1))

(defun ebdb-merge-lists (l1 l2 cmp)
  "Merge two lists L1 and L2 based on comparison CMP.
An element from L2 is added to L1 if CMP returns nil for all elements of L1.
If L1 or L2 are not lists, they are replaced by (list L1) and (list L2)."
  (let (merge)
    (unless (listp l1) (setq l1 (list l1)))
    (dolist (e2 (if (listp l2) l2 (list l2)))
      (let ((ll1 l1) e1 fail)
        (while (setq e1 (pop ll1))
          (if (funcall cmp e1 e2)
              (setq ll1 nil
                    fail t)))
        (unless fail (push e2 merge))))
    (append l1 (nreverse merge))))

;;; Parsing other things

(defun ebdb-divide-name (string)
  "Divide STRING into a first name and a last name.
Case is ignored.  Return name as (FIRST . LAST).
LAST is always a string (possibly empty).  FIRST may be nil."
  (let ((case-fold-search t)
        first suffix)
    ;; Separate a suffix.
    (if (string-match ebdb-lastname-suffix-re string)
        (setq suffix (concat " " (match-string 1 string))
              string (substring string 0 (match-beginning 0))))
    (cond ((string-match "\\`\\(.+\\),[ \t\n]*\\(.+\\)\\'" string)
           ;; If STRING contains a comma, this probably means that STRING
           ;; is of the form "Last, First".
           (setq first (match-string 2 string)
                 string (match-string 1 string)))
          ((string-match ebdb-lastname-re string)
           (setq first (and (not (zerop (match-beginning 0)))
                            (substring string 0 (match-beginning 0)))
                 string (match-string 1 string))))
    (cons (and first (ebdb-string-trim first))
          (ebdb-string-trim (concat string suffix)))))

(defun ebdb-parse-postcode (string)
  "Check whether STRING is a legal postcode.
Do this only if `ebdb-check-postcode' is non-nil."
  (if ebdb-check-postcode
      (let ((postcodes ebdb-legal-postcodes) re done)
        (while (setq re (pop postcodes))
          (if (string-match re string)
              (setq done t postcodes nil)))
        (if done string
          (signal 'ebdb-unparseable (list string))))
    string))

(defsubst ebdb-record-lessp (record1 record2)
  (string< (ebdb-record-sortkey record1)
           (ebdb-record-sortkey record2)))


;;; Reading and Writing the EBDB

;;;###autoload
(defun ebdb-load ()
  "Load all databases listed in `ebdb-sources'.  All the
important work is done by the `ebdb-db-load' method."
  (let ((sources (if (listp ebdb-sources)
		     ebdb-sources
		   (list ebdb-sources))))
    ;; Check if we're re-loading.
    (when (and ebdb-db-list
	       (object-assoc t 'dirty ebdb-db-list))
      ;; Later we'll give users the option to discard unsaved data.
      (error "Databases have unsaved data, save first."))
    (message "Loading EBDB sources...")
    (ebdb-clear-vars)
    (run-hooks 'ebdb-before-load-hook)
    (dolist (s sources)
      (if (stringp s)
	  (if (file-exists-p s)
	      ;; Handle auto-saved databases.
	      (let ((auto-save-file (ebdb-db-make-auto-save-file-name s))
		    (orig-filename s))
		(if (and (file-exists-p auto-save-file)
			 (yes-or-no-p (format "Recover auto-save file for %s? " s)))
		    (progn (setq s (eieio-persistent-read auto-save-file 'ebdb-db t))
			   (setf (slot-value s 'file) orig-filename)
			   (setf (slot-value s 'dirty) t))
		  (setq s (eieio-persistent-read s 'ebdb-db t))))
	    ;; Handle new/nonexistent databases.
	    (when (yes-or-no-p (format "%s does not exist, create? " s))
	      (setq s (make-instance 'ebdb-db-file :file s :dirty t))
	      ;; Try to get it on disk first.
	      (ebdb-db-save s)))
	(error "Source %s should be a filename." s))
      ;; Now load it.
      (if (child-of-class-p (eieio-object-class s) 'ebdb-db)
	  (if (null (slot-value s 'disabled))
	      (ebdb-db-load s)
	    (message "Database %s is currently disabled." s)
	    (sit-for 2))
	(error "Object %s is not a EBDB database" s)))
    (if (and
	 (null ebdb-record-tracker)
	 (bound-and-true-p bbdb-file)
	 (file-exists-p bbdb-file))
	;; We're migrating from a previous version of EBDB.
	(ebdb-migrate-from-bbdb)
      (message "Loading EBDB sources... done"))
    (ebdb-initialize)
    ;; Users will expect the same ordering as `ebdb-sources'
    (setq ebdb-db-list (nreverse ebdb-db-list))
    (run-hooks 'ebdb-after-load-hook)
    (length ebdb-record-tracker)))

;; If we wanted to make it seem like EBDB was loading faster, we could
;; defer calls to `ebdb-initialize' until the first time the database
;; is searched.
(defun ebdb-initialize ()
  "After all databases are loaded, initialize the records.

This results in the creation of all the secondary data
structures: label lists, `ebdb-org-hashtable', record caches,
etc."
  (mapcar #'ebdb-init ebdb-record-tracker))

(defun ebdb-address-continental-p (address)
  "Return non-nil if ADDRESS is a continental address.
This is done by comparing the postcode to `ebdb-continental-postcode-regexp'.

This is a possible identifying function for
`ebdb-address-format-list' and `ebdb-print-address-format-list'."
  (string-match ebdb-continental-postcode-regexp
                (ebdb-address-postcode address)))

;; This function can provide some guidance for writing
;; your own address formatting function
(defun ebdb-format-address-default (address)
  "Return formatted ADDRESS as a string.
This is the default format; it is used in the US, for example.
The result looks like this:
       label: street
              street
              ...
              locality, region postcode
              country.

This function is a possible formatting function for
`ebdb-address-format-list'."
  (let ((country (ebdb-address-country address))
        (streets (ebdb-address-streets address)))
    (when (symbolp country)
      (setq country (car (rassq country ebdb-i18n-countries))))
    (concat (if streets
                (concat (mapconcat 'identity streets "\n") "\n"))
            (ebdb-concat ", " (ebdb-address-locality address)
                         (ebdb-concat " " (ebdb-address-region address)
                                      (ebdb-address-postcode address)))
            (unless (or (not country) (string= "" country))
              (concat "\n" country)))))

(defun ebdb-format-address (address layout)
  "Format ADDRESS using LAYOUT.  Return result as a string.
The formatting rules are defined in `ebdb-address-format-list'."
  (let ((list ebdb-address-format-list)
        (country (ebdb-address-country address))
        elt string)
    (while (and (not string) (setq elt (pop list)))
      (let ((identifier (car elt))
            (format (nth layout elt))
            ;; recognize case for format identifiers
            case-fold-search str)
        (when (or (eq t identifier) ; default
                  (and (functionp identifier)
                       (funcall identifier address))
                  (and country
                       (listp identifier)
                       ;; ignore case for countries
                       (assq country identifier)))
          (cond ((functionp format)
                 (setq string (funcall format address)))
                ((stringp format)
                 (setq string "")
                 (dolist (form (split-string (substring format 1 -1)
                                             (substring format 0 1) t))
                   (cond ((string-match "%s" form) ; street
                          (mapc (lambda (s) (setq string (concat string (format form s))))
                                (ebdb-address-streets address)))
                         ((string-match "%l" form) ; locality
                          (unless (or (not (setq str (ebdb-address-locality address))) (string= "" str))
                            (setq string (concat string (format (replace-regexp-in-string "%l" "%s" form) str)))))
                         ((string-match "%p" form) ; postcode
                          (unless (or (not (setq str (ebdb-address-postcode address))) (string= "" str))
                            (setq string (concat string (format (replace-regexp-in-string "%p" "%s" form) str)))))
                         ((string-match "%r" form) ; region
                          (unless (or (not (setq str (ebdb-address-region address))) (string= "" str))
                            (setq string (concat string (format (replace-regexp-in-string "%r" "%s" form t) str)))))
                         ((string-match "%c" form) ; country
			  (when (symbolp country)
			    (setq country (car (rassq country ebdb-i18n-countries))))
                          (unless (or (not country) (string= "" country))
                            (setq string (concat string (format (replace-regexp-in-string "%c" "%s" form t) country)))))
                         (t (error "Malformed address format element %s" form)))))
                (t (error "Malformed address format %s" format))))))
    (unless string
      (error "No match of `ebdb-address-format-list'"))
    string))

;; Loading and saving EBDB

(defun ebdb-save (&optional prompt)
  "Save the EBDB if it is modified.
If PROMPT is non-nil prompt before saving."
  (interactive (list nil))
  ;; TODO: Reimplement ebdb-remote-file, or otherwise do something
  ;; about that.
  (message "Saving the EBDB...")
  (dolist (s ebdb-db-list)
    (ebdb-db-save s prompt))
  (message "Saving the EBDB... done"))

;;;###autoload
(defun ebdb-version (&optional arg)
  "Return string describing the version of EBDB.
With prefix ARG, insert string at point."
  (interactive (list (or (and current-prefix-arg 1) t)))
  (let ((version-string (format "EBDB version %s (%s)"
                                ebdb-version ebdb-version-date)))
    (cond ((numberp arg) (insert (message version-string)))
          ((eq t arg) (message version-string))
          (t version-string))))


;;; Searching EBDB

(defvar ebdb-search-invert nil
  "Bind this variable to t in order to invert the result of `ebdb-search'.")

(defun ebdb-search-invert-p ()
  "Return variable `ebdb-search-invert' and set it to nil.
To set it again, use command `ebdb-search-invert'."
  (let ((result ebdb-search-invert))
    (setq ebdb-search-invert nil)
    (aset ebdb-modeline-info 2 nil)
    (aset ebdb-modeline-info 3 nil)
    result))

;;;###autoload
(defun ebdb-search-invert (&optional arg)
  "Toggle inversion of the next search command.
With prefix ARG a positive number, invert next search.
With prefix ARG a negative number, do not invert next search."
  (interactive "P")
  (setq ebdb-search-invert
        (or (and (numberp arg) (< 0 arg))
            (and (not (numberp arg)) (not ebdb-search-invert))))
  (aset ebdb-modeline-info 2 (if ebdb-search-invert "inv"))
  (aset ebdb-modeline-info 3 (if ebdb-search-invert
                                 (substitute-command-keys
                                  "\\<ebdb-mode-map>\\[ebdb-search-invert]")))
  (ebdb-prefix-message))

(defmacro ebdb-search (records &optional name-re org-re mail-re notes-re
			       user-field-re phone-re address-re)
  "Search RECORDS for fields matching regexps.
Regexp NAME-RE is matched against FIRST_LAST, LAST_FIRST, and
AKA.  Regexp USER-FIELD-RE is matched against user fields.
USER-FIELD-RE may also be a cons (LABEL . RE).  Then RE is
matched against field LABEL.  If LABEL is '* then RE is matched
against any user field.

This macro only generates code for those fields actually being searched for;
literal nils at compile-time cause no code to be generated.

To reverse the search, bind variable `ebdb-search-invert' to t.

See also `ebdb-message-search' for fast searches using `ebdb-hashtable'
but not allowing for regexps."
  (let (clauses)
    ;; I did not protect these vars from multiple evaluation because that
    ;; actually generates *less efficient code* in elisp, because the extra
    ;; bindings cannot easily be optimized away without lexical scope.  fmh.
    (or (stringp name-re) (symbolp name-re) (error "name-re must be atomic"))
    (or (stringp org-re) (symbolp org-re) (error "org-re must be atomic"))
    (or (stringp mail-re) (symbolp mail-re) (error "mail-re must be atomic"))
    (or (stringp user-field-re) (symbolp user-field-re) (consp user-field-re)
        (error "user-field-re must be atomic or cons"))
    (or (stringp phone-re) (symbolp phone-re) (error "phone-re must be atomic"))
    (or (stringp address-re) (symbolp address-re) (error "address-re must be atomic"))
    (when name-re
      (push `(ebdb-record-search record 'name ,name-re) clauses))
    (when org-re
      (push `(let ((organizations (when (slot-exists-p record 'organizations)
				    (slot-value record 'organizations)))
		   org done)
	       (if organizations
		   (while (and (setq org (pop organizations)) (not done))
		     (setq done (string-match ,org-re (ebdb-string org))))
		 ;; so that "^$" can be used to find records that
		 ;; have no organization
		 (setq done (string-match ,org-re "")))
	       done)
	    clauses))

    (when phone-re
      (push `(ebdb-record-search record 'phone ,phone-re) clauses))
    (when address-re
      (push `(ebdb-record-search record 'address ,address-re) clauses))
    (when mail-re
      (push `(ebdb-record-search record 'mail ,mail-re) clauses))
    (when notes-re
      (push `(ebdb-record-search record 'notes ,notes-re) clauses))
    (when user-field-re
      (push `(ebdb-record-search record 'user ,user-field-re) clauses))
    `(let ((case-fold-search ebdb-case-fold-search)
           (invert (ebdb-search-invert-p))
           matches)
       (dolist (record ,records)
         (unless (eq (not invert) (not (or ,@clauses)))
           (push record matches)))
       (nreverse matches))))

(defun ebdb-search-read (&optional field)
  "Read regexp to search FIELD values of records."
  (read-string (format "Search records%s %smatching regexp: "
                       (if field (concat " with " field) "")
                       (if ebdb-search-invert "not " ""))))

(provide 'ebdb)
;;; ebdb.el ends here
