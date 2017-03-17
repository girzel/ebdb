;;; ebdb-vcard.el --- vCard export and import routine for EBDB  -*- lexical-binding: t; -*-

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

;; This file contains formatting and parsing functions used to export
;; EBDB contacts to vcard format, and to create contacts from vcard
;; files.  It only supports VCard versions 3.0 and 4.0.

;; https://tools.ietf.org/html/rfc6350

;;; Code:

(require 'ebdb-format)

(defclass ebdb-formatter-vcard (ebdb-formatter)
  ((coding-system :initform 'utf-8-dos)
   (version-string
    :type string
    :allocation :class
    :documentation "The string to insert for this formatter's
    version."))
  :abstract t
  :documentation "Base formatter for vCard export.")

(defclass ebdb-formatter-vcard-30 (ebdb-formatter-vcard)
  ((version-string
    :initform "3.0"))
  :documentation "Formatter for vCard format 3.0.")

(defclass ebdb-formatter-vcard-40 (ebdb-formatter-vcard)
  ((version-string
    :initform "4.0"))
  :documentation "Formatter for vCard format 4.0.")

(defgroup ebdb-vcard nil
  "Customization options for EBDB vCard support."
  :group 'ebdb)

(defcustom ebdb-vcard-default-40-formatter
  (make-instance 'ebdb-formatter-vcard-40
		 :object-name "VCard 4.0 (default)"
		 :combine nil
		 :collapse nil
		 :include '(ebdb-field-uuid
			    ebdb-field-timestamp
			    ebdb-field-mail
			    ebdb-field-name
			    ebdb-field-address
			    ebdb-field-url
			    ebdb-field-role
			    ebdb-field-anniversary
			    ebdb-field-relation
			    ebdb-field-phone
			    ebdb-field-notes
			    ebdb-org-field-tags)
		 :header nil)
  "The default formatter for VCard 4.0 exportation."
  :group 'ebdb-vcard
  :type 'ebdb-formatter-vcard)

(defcustom ebdb-vcard-default-30-formatter
  (make-instance 'ebdb-formatter-vcard-30
		 :object-name "VCard 3.0 (default)"
		 :combine nil
		 :collapse nil
		 :include '(ebdb-field-uuid
			    ebdb-field-timestamp
			    ebdb-field-mail
			    ebdb-field-name
			    ebdb-field-address
			    ebdb-field-url
			    ebdb-field-role
			    ebdb-field-anniversary
			    ebdb-field-relation
			    ebdb-field-phone
			    ebdb-field-notes
			    ebdb-org-field-tags)
		 :header nil)
  "The default formatter for VCard 3.0 exportation."
  :group 'ebdb-vcard
  :type 'ebdb-formatter-vcard)

(defcustom ebdb-vcard-label-alist
  '(("REV" . ebdb-field-timestamp)
    ("NOTE" . ebdb-field-notes)
    ("X-CREATION-DATE" . ebdb-field-creation-date)
    ("UID" . ebdb-field-uuid)
    ("EMAIL" . ebdb-field-mail)
    ("TEL" . ebdb-field-phone)
    ("RELATED" . ebdb-field-relation)
    ("CATEGORIES" . ebdb-org-field-tags)
    ("BDAY" . ebdb-field-anniversary)
    ("ANNIVERSARY" . ebdb-field-anniversary)
    ("URL" . ebdb-field-url)
    ("ADR" . ebdb-field-address)
    ("NICKNAME" . ebdb-field-name-complex))
  "Correspondences between VCard properties and EBDB field classes.

This alist is neither exhaustive nor authoritative.  It's purpose
is to simplify property labeling during the export process, and
to classify properties during import.  The import process does
not always respect these headings."

  :group 'ebdb-vcard
  :type '(alist :key-type string :value-type symbol))

(defcustom ebdb-vcard-kind-class-alist
  '(("individual" . ebdb-record-person)
    ("org" . ebdb-record-organization))
  "Alist holding correspondences between vCard 4.0 KIND property
  values and EBDB record classes."
  :group 'ebdb-vcard
  :type '(alist :key-type string :value-type symbol))

(define-error 'ebdb-unparseable-vcard "vCard version not supported" 'ebdb-unparseable)

(defsubst ebdb-vcard-escape (str)
  "Escape commas, semi-colons and newlines in STR."
  (replace-regexp-in-string
   "\\([^\\]\\)\\([\n]+\\)" "\\1\\\\n"
   (replace-regexp-in-string "\\([^\\]\\)\\([,;]\\)" "\\1\\\\\\2" str)))

(defsubst ebdb-vcard-unescape (str)
  "Unescape escaped commas, semicolons and newlines in STR."
  (replace-regexp-in-string
   "\\\\n" "\n"
   (replace-regexp-in-string
    "\\\\\\([,;]\\)" "\\1" str)))

;; The RFC says fold any lines longer than 75 octets, excluding the
;; line break.  Folded lines are delineated by a CRLF plus a space or
;; tab.  Multibyte characters must not be broken.

;; TODO: This implementation assumes that Emacs' internal coding
;; system is similar enough to the utf-8 that the file will eventually
;; be written in that `string-bytes' (which returns a length according
;; to Emacs' own coding) will map accurately to what eventually goes
;; in the file.  Eli notes this is not really true, and could result
;; in unexpected behavior, and he recommends using
;; `filepos-to-bufferpos' instead.  Presumably that would involve
;; /first/ writing the vcf file, then backtracking and checking for
;; correctness.
(defun ebdb-vcard-fold-lines (text)
  "Fold lines in TEXT, which represents a vCard contact."
  (let ((lines (split-string text "\n"))
	outlines)
    (dolist (l lines)
      (while (> (string-bytes l) 75)	; Line is too long.
	(if (> (string-bytes l) (length l))
	    ;; Multibyte characters.
	    (let ((acc (string-to-vector l)))
	      (setq l nil)
	      (while (> (string-bytes (concat acc)) 75)
		;; Pop characters off the end of acc and stick them
		;; back in l, until acc is short enough to go in
		;; outlines.  Probably hideously inefficient.
		(push (aref acc (1- (length acc))) l)
		(setq acc (substring acc 0 -1)))
	      (push acc outlines)
	      (setq l (concat " " l)))
	  ;; No multibyte characters.
	  (push (substring l 0 75) outlines)
	  (setq l (concat " " (substring l 75)))))
      (push l outlines))
    (mapconcat #'identity (nreverse outlines) "\n")))

(defun ebdb-vcard-unfold-lines (text)
  "Unfold lines in TEXT, which represents a vCard contact."
  (replace-regexp-in-string "\n[\s\t]" "" text))

(cl-defmethod ebdb-fmt-process-fields ((_f ebdb-formatter-vcard)
				       (_record ebdb-record)
				       field-list)
  field-list)

(cl-defmethod ebdb-fmt-process-fields ((fmt ebdb-formatter-vcard)
				       (record ebdb-record-person)
				       field-list)
  "Process fields in FIELD-LIST.

All this does is split role instances into multiple fields."
  (let (org out-list)
    (dolist (f field-list)
      (if (object-of-class-p f 'ebdb-field-role)
	  ;; Split it apart.
	  (with-slots (org-uuid mail fields defunct) f
	    (unless defunct
	      (setq org (ebdb-gethash org-uuid 'uuid))
	      ;; Store the name of the organization in the TYPE
	      ;; parameter of the various properties.  I'd rather
	      ;; stick a UUID somewhere, but haven't immediately
	      ;; figured out how that would be done.
	      (push (cons "ORG"
			  (ebdb-record-name org))
		    out-list)
	      (push (cons (format "TITLE;TYPE=\"%s\"" (ebdb-record-name org))
			  (slot-value f 'object-name))
		    out-list)
	      (when (or mail fields)
		(dolist (elt (cons mail fields))
		  (push (cons
			 (format "%s;%s"
				 (ebdb-fmt-field-label fmt elt 'normal record)
				 (format "TYPE=\"%s\"" (ebdb-record-name org)))
			 (ebdb-fmt-field fmt elt 'normal record))
			out-list)))))
	(push f out-list)))
    out-list))

(cl-defmethod ebdb-fmt-record ((f ebdb-formatter-vcard)
			       (r ebdb-record))
  "Format a single record R in VCARD format."
  ;; Because of the simplicity of the VCARD format, we only collect
  ;; the fields, there's no need to sort them, and the only processing
  ;; that happens is for role fields.
  (let ((fields (ebdb-fmt-process-fields
		 f r
		 (ebdb-fmt-collect-fields f r)))
	header-fields body-fields)
    (setq header-fields
	  (list (slot-value r 'name))
	  body-fields
	  (mapcar
	   (lambda (fld)
	     ;; This is a silly hack, but...
	     (if (consp fld)
		 fld
	       (cons (ebdb-fmt-field-label f fld 'normal r)
		     (ebdb-fmt-field f fld 'normal r))))
	   fields))
    (concat
     (format "BEGIN:VCARD\nVERSION:%s\n"
	     (slot-value f 'version-string))
     (ebdb-fmt-record-header f r header-fields)
     (ebdb-fmt-record-body f r body-fields)
     "\nEND:VCARD\n")))

(cl-defmethod ebdb-fmt-record-header ((f ebdb-formatter-vcard)
				      (r ebdb-record)
				      (fields list))
  "Format the header of a VCARD record.

VCARDs don't really have the concept of a \"header\", so this
method is just responsible for formatting the record name."
  (let ((name (car fields)))
   (concat
    (format "FN:%s\n" (ebdb-string name))
    (format "N;SORT-AS=\"%s\":%s\n"
	    (ebdb-record-sortkey r)
	    (ebdb-fmt-field f name 'normal r)))))

(cl-defmethod ebdb-fmt-record-body ((_f ebdb-formatter-vcard)
				    (_r ebdb-record)
				    (fields list))
  (mapconcat
   (lambda (f)
     (format "%s:%s"
	     (car f) (cdr f)))
   fields
   "\n"))

(cl-defmethod ebdb-fmt-record-body :around ((_f ebdb-formatter-vcard-40)
					    (_r ebdb-record-person)
					    (_fields list))
  (let ((str (cl-call-next-method)))
    (concat str "\nKIND:individual")))

(cl-defmethod ebdb-fmt-record-body :around ((_f ebdb-formatter-vcard-40)
					    (_r ebdb-record-organization)
					    (_fields list))
  (let ((str (cl-call-next-method)))
    (concat str "\nKIND:org")))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (field ebdb-field)
			      _style
			      _record)
  (ebdb-vcard-escape (ebdb-string field)))

(cl-defmethod ebdb-fmt-field-label ((_f ebdb-formatter-vcard)
				    (field ebdb-field)
				    _style
				    _record)
  (car (rassoc (eieio-class-name
		(eieio-object-class field))
	       ebdb-vcard-label-alist)))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (mail ebdb-field-mail)
			      _style
			      _record)
  (slot-value mail 'mail))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (ts ebdb-field-timestamp)
			      _style
			      _record)
  (format-time-string "%Y%m%dT%H%M%S%z" (slot-value ts 'timestamp) t))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (name ebdb-field-name-complex)
			      _style
			      (record ebdb-record))
  (let ((sort-as (slot-value (ebdb-record-cache record) 'sortkey)))
   (with-slots (surname given-names prefix suffix) name
     (format
      "SORT-AS;:%s;%s;%s;%s;%s"
      sort-as
      (or surname "")
      (if given-names (car given-names) "")
      (if (cdr given-names) (mapconcat #'identity (cdr given-names) ",") "")
      (or prefix "")
      (or suffix "")))))

(cl-defmethod ebdb-fmt-field-label ((_f ebdb-formatter-vcard)
				    (_field ebdb-field-uuid)
				    _style
				    _record)
  (concat (cl-call-next-method) ":urn:uuid"))

(cl-defmethod ebdb-fmt-field-label ((_f ebdb-formatter-vcard)
				    (mail ebdb-field-mail)
				    _style
				    _record)
  (with-slots (priority) mail
    (concat
     (cl-call-next-method)
     (cl-case priority
       ('primary ";PREF=1")
       ('normal ";PREF=10")
       ('defunct ";PREF=100")
       (t "")))))

(cl-defmethod ebdb-fmt-field-label ((_f ebdb-formatter-vcard)
			 	    (field ebdb-field-labeled)
				    _style
				    _record)
  (let ((ret (cl-call-next-method)))
    (if-let ((lab (slot-value field 'object-name)))
	(concat ret
		";TYPE=" (ebdb-vcard-escape lab))
      ret)))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (addr ebdb-field-address)
			      _style
			      _record)
  (with-slots (streets locality region postcode country) addr
    (concat ";;"
	    (mapconcat
	     #'ebdb-vcard-escape
	     streets ",")
	    (format
	     ";%s;%s;%s;%s"
	     (or locality "")
	     (or region "")
	     (or postcode "")
	     (or country "")))))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (rel ebdb-field-relation)
			      _style
			      _record)
  (concat "urn:uuid:" (slot-value rel 'rel-uuid)))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (tags ebdb-org-field-tags)
			      _style
			      _record)
  (ebdb-concat "," (slot-value tags 'tags)))

(cl-defmethod ebdb-fmt-field-label ((_f ebdb-formatter-vcard)
				    (ann ebdb-field-anniversary)
				    _style
				    _record)
  (let* ((label (slot-value ann 'object-name))
	 (label-string
	  (if (string= label "birthday")
	      "BDAY"
	    (concat "ANNIVERSARY;TYPE=" label))))
    (concat label-string (format ";CALSCALE=%s"
				 (slot-value ann 'calendar)))))

(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (ann ebdb-field-anniversary)
			      _style
			      _record)
  (pcase-let ((`(,month ,day ,year)
	       (calendar-gregorian-from-absolute
		(slot-value ann 'date))))
    (format "%d%02d%02d" year month day)))

;; Parsing vCard

(defun ebdb-vcard-parse-file (file load &optional noerror)
  "Parse FILE as a *.vcf vCard file.

See `ebdb-vcard-parse-buffer' for the meaning of optional
arguments LOAD and NOERROR."
  (interactive
   (list (read-file-name "Load contacts from file: " nil nil t)
	 (ebdb-prompt-for-db)))
  ;; Don't do any checking of FILE, just read it and assume it's
  ;; vCard.  Raise errors if necessary.
  (with-temp-buffer
    (insert-file-contents file)
    (ebdb-vcard-parse-buffer load noerror)))

(defun ebdb-vcard-parse-buffer (&optional load noerror)
  "Parse the current buffer as a vCard file.

Look for individual record boundaries and pass chunks of text to
`ebdb-vcard--dispatch', which passes the text to the appropriate
`ebdb-parse-record' method.

The return value is a list of EBDB record instances.  If optional
argument LOAD is given, the records are also loaded into a
database.  LOAD can be t, in which case the user may be prompted
for a database to use, or it can be an actual database instance.

By default, unparseable records will raise an error.  If optional
argument NOERROR is t, those records will be silently discarded.
If it is the symbol 'message, print a message after parsing about
the number of unusable records.  If it is the symbol 'buffer, pop
up a buffer containing the unparseable text."
  (interactive)
  (let ((case-fold-search t)
	(unparseable (cl-case noerror
		       (buffer
			"Unparseable records:\n\n")
		       (message
			0)
		       (t nil)))
	records chunk)
    (goto-char (point-min))
    (while (re-search-forward "begin:vcard\n" (point-max) t)

      (setq chunk
	    (buffer-substring
	     (point)
	     (save-excursion
	       (re-search-forward "end:vcard\n" (point-max))
	       (forward-line -1)
	       (point))))

      (condition-case err
	  (push
	   (ebdb-vcard--dispatch chunk)
	   records)
	(ebdb-unparseable-vcard
	 (cond
	  ((eql noerror 'buffer)
	   (setq unparseable (concat unparseable "\n\n" chunk)))
	  ((eql noerror 'message)
	   (cl-incf unparseable))
	  (noerror
	   nil)
	  (t (signal 'ebdb-unparseable-vcard (list err)))))))
    
    (when load
      (let ((db (cond ((and (eieio-object-p load)
			    (object-of-class-p load ebdb-db))
		       load)
		      ((= 1 (length ebdb-db-list))
		       (car ebdb-db-list)
		       (t
			(ebdb-prompt-for-db))))))
	(dolist (r records)
	  (ebdb-db-add-record db r))
	(message "Imported %d records to %S"
		 (length records)
		 db)))
    
    (when unparseable
      (cond ((eql noerror 'message)
	     (message "%d unparseable vCard records ignored" unparseable))
	    ((eql noerror 'buffer)
	     (pop-to-buffer
	      (get-buffer-create "*EBDB Parse Errors*")
	      '(nil . ((window-height . 10))))
	     (insert unparseable)
	     (fit-window-to-buffer)
	     (goto-char (point-min)))
	    ;; We shouldn't get here; at this stage NOERROR should
	    ;; only be one of the above symbols.
	    (t nil)))

    records))

(defun ebdb-vcard--dispatch (text)
  "Dispatches TEXT (a vCard) to the appropriate `ebdb-parse-record' method.

TEXT should be the text of a vCard contact _without_ the BEGIN
and END lines.  This function assumes the first line will contain
a VERSION property, and uses that and other information to choose
an appropriate vCard formatter to use in parsing, or else signals
`ebdb-unparseable-vcard'.

Returns the results of the call to `ebdb-parse-record', with the
expectation that that will be a `ebdb-record' class instance.
Does not handle any `ebdb-unparseable-vcard' errors raised in
that method."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    ;; Right now, we just choose between 3.0 and 4.0.  Later, we'll
    ;; check for Apple/Google/Microsoft-specific vCard flavors, and
    ;; handle those here, too.
    (let* ((version (aref (ebdb-vcard-extract-property "version") 2))
	   (fmt (cond
		 ((string= version "3.0")
		  ebdb-vcard-default-30-formatter)
		 ((string= version "4.0")
		  ebdb-vcard-default-40-formatter)
		 (t (signal 'ebdb-unparseable-vcard
			    (list (format
				   "Unparseable vCard version: %s"
				   version)))))))
      (ebdb-parse-record fmt))))

(defun ebdb-vcard-extract-property (&optional prop)
  "Find and extract a single vcard property in the current
  buffer.

This can be used in one of two ways: with no optional PROP, it
simply treats the current line as a vcard property and returns
that, leaving point at the beginning of the next line.  With
PROP, it searches for that property in particular, removes the
line from the buffer altogether, then returns to the starting
point.

The return value is a three-element vector of [prop-name
paramters value].  The parameters element is a list, each element
of which is either a string, for parameters with no value, or a
cons cell of (param-name . value).

This function is also responsible for transforming or decoding
values and parameters as necessary."
  (let* ((line
	  (if prop
	      (save-excursion
		(re-search-forward
		 (concat "^" prop "[:;]") (point-max) t)
		(delete-and-extract-region
		 (point-at-bol)
		 (progn (forward-line) (point-at-bol))))
	    (buffer-substring-no-properties
	     (point-at-bol)
	     (progn (forward-line)
		    (point)))))
	 (parts (split-string line ":"))
	 (val (mapconcat #'string-trim (cdr parts) ":"))
	 (prop-and-param (split-string (car parts) ";"))
	 (prop (car prop-and-param))
	 (params (cdr prop-and-param)))
    (when params
      (setq params (ebdb-vcard-treat-params params)))
    (vector prop params val)))

(defun ebdb-vcard-treat-params (params)
  "Handle a list of parameters.

Return the treated list.  Single-term paramters are returned as a
single string.  Key/value parameters are returned as a cons cell
of (key . value)."
  (mapcar
   (lambda (el)
     (pcase (split-string el "=")
       (`(and ,el (stringp))
	el)
       (`(,param ,value)
	(cons param value))))
   params))

(cl-defmethod ebdb-parse-record ((fmt ebdb-formatter-vcard-40))
  "For v4.0, first use the KIND property to set record class."
  ;; Do the name here!  And have another method that does the name for
  ;; 3.0.  That way we can set it properly, and then the base method
  ;; can just go about its business.
  (let* ((kind (downcase
		(cdr (ebdb-vcard-extract-property "kind"))))
	 (record-class (cdr (assoc kind ebdb-vcard-kind-class-alist))))
    (cl-call-next-method fmt record-class)))

(cl-defmethod ebdb-parse-record ((fmt ebdb-formatter-vcard)
				 &optional record-class fields)
  "Parse the current buffer as vCard text, using FMT.

FMT is an instance of `ebdb-formatter-vcard', meaning that at
this point we should already have detected the vCard version, and
chosen the correct formatter."
  (let* ((record-class (or record-class ebdb-default-record-class))
	 ;; Something needs refactoring here.  Probably records should
	 ;; be allowed to parse their own names.
	 (name-class (if (child-of-class-p record-class 'ebdb-record-person)
			 'ebdb-field-name-complex
		       'ebdb-field-name-simple))
	 (name (ebdb-parse-vcard-field fmt name-class
				       (aref (ebdb-vcard-extract-property "n") 2)))
	 (record (make-instance record-class :name name))
	 chunks unusable-fields field-class)
    (goto-char (point-min))
    (while (null (eobp))
      (push (ebdb-vcard-extract-property) chunks))
    ;; Handle role fields here.
    (pcase-dolist (`[,label ,params ,value] chunks)
      (setq field-class (cdr-safe (assoc-string label ebdb-vcard-label-alist)))
      (or (and field-class
	       (condition-case nil
		   (push (ebdb-parse-vcard-field
			  fmt field-class value params)
			 fields)
		 (ebdb-unparseable nil)))
	  (push (vector label params value) unusable-fields)))
    ;; We'll want to do more examining of unusable-fields than this,
    ;; but for now just treat anything with an X- prefix as a simple
    ;; user field.
    (dolist (f unusable-fields)
      (when (string-prefix-p "X-" (aref f 0))
	(push (make-instance 'ebdb-field-user-simple
			     :object-name (aref f 0)
			     :value (aref f 2))
	      fields)))
    (dolist (f fields)
      (condition-case nil
	  (ebdb-record-insert-field record f)
	(ebdb-unacceptable-field
	 (push f unusable-fields))))
    record))

(cl-defgeneric ebdb-parse-vcard-field (fmt field-class string &optional params)
  "Parse STRING as instance of FIELD-CLASS, as specified by FMT.

PARAMS, if present, is a list of parameters.")

(cl-defmethod ebdb-parse-vcard-field ((_fmt ebdb-formatter-vcard)
				      (cls (subclass ebdb-field-labeled))
				      (str string)
				      &optional params)
  (let ((inst (cl-call-next-method)))
   (when-let ((label (cdr-safe (assoc-string "type" params))))
     (setf (slot-value inst 'object-name) label))
   inst))

(cl-defmethod ebdb-parse-vcard-field ((_fmt ebdb-formatter-vcard)
				      (cls (subclass ebdb-field-name-complex))
				      (str string)
				      &optional _params)
  "Parse a vCard N property value."
  
  (pcase-let ((`(,surname ,given ,given-cont ,prefix ,suffix)
	       (split-string str ";")))
    (make-instance
     cls
     :surname surname
     :given-names (cons given (split-string given-cont ","))
     :prefix prefix
     :suffix suffix)))

(cl-defmethod ebdb-parse-vcard-field ((_fmt ebdb-formatter-vcard)
				      (cls (subclass ebdb-field-address))
				      (str string)
				      &optional _params)
  "Parse a vCard ADR property value."
  (pcase-let ((`(,po ,ext ,streets ,local ,region ,pc ,country)
	       (split-string str ";")))
    (when po
      (setf (car streets) (concat po (car streets))))
    (when ext
      (push ext streets))
    (make-instance
     cls
     :streets streets
     :locality local
     :region region
     :postcode pc
     :country country)))

(cl-defmethod ebdb-parse-vcard-field ((_fmt ebdb-formatter-vcard)
				      (cls (subclass ebdb-field))
				      (str string)
				      &optional _params)
  (ebdb-parse cls str))

(provide 'ebdb-vcard)
;;; ebdb-vcard.el ends here
