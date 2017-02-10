;;; ebdb-vcard.el --- Vcard export and import routine for EBDB  -*- lexical-binding: t; -*-

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
;; files.

;;; Code:

(defclass ebdb-formatter-vcard (ebdb-formatter)
  ((version-string
    :type string
    :allocation :class
    :documentation "The string to insert for this formatter's
    version."))
  :abstract t
  :documentation "Base formatter for VCard export.")

(defclass ebdb-formatter-vcard-30 (ebdb-formatter-vcard)
  ((version-string
    :initform "3.0"))
  :documentation "Formatter for VCard format 3.0.")

(defclass ebdb-formatter-vcard-40 (ebdb-formatter-vcard)
  ((version-string
    :initform "4.0")
   (coding-system :initform 'utf-8-emacs))
  :documentation "Formatter for VCard format 4.0.")

(defgroup ebdb-vcard nil
  "Customization options for EBDB Vcard support."
  :group 'ebdb)

(defcustom ebdb-vcard-default-formatter
  (make-instance 'ebdb-formatter-vcard-40
		 :object-name "VCard 4.0"
		 :combine nil
		 :collapse nil
		 :include '(ebdb-field-uuid
			    ebdb-field-timestamp
			    ebdb-field-mail
			    ebdb-field-url
			    ebdb-field-anniversary
			    ebdb-field-relation
			    ebdb-field-phone
			    ebdb-field-notes)
		 :header nil)
  "The default formatter for vcard exportation."
  :group 'ebdb-vcard
  :type 'ebdb-formatter-vcard)


(cl-defmethod ebdb-fmt-record ((f ebdb-formatter-vcard)
			       (r ebdb-record))
  "Format a single record R in VCARD format."
  ;; Because of the simplicity of the VCARD format, we only collect
  ;; the fields, there's no need to sort or "process" them.
  (let ((fields (ebdb-fmt-collect-fields f r))
	header-fields body-fields)
    (setq header-fields
	  (list (slot-value r 'name))
	  body-fields
	  (mapcar
	   (lambda (fld)
	     (cons (ebdb-fmt-field-label f fld 'normal r)
		   (ebdb-fmt-field f fld 'normal r)))
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
    (format "N:%s\n"
	    (ebdb-fmt-field f name 'normal r)))))

(cl-defmethod ebdb-fmt-record-body ((f ebdb-formatter-vcard)
				    (r ebdb-record)
				    (fields list))
  (mapconcat
   (lambda (f)
     (format "%s:%s"
	     (car f) (cdr f)))
   fields
   "\n"))

(cl-defmethod ebdb-fmt-record-body :around ((f ebdb-formatter-vcard-40)
					    (r ebdb-record-person)
					    (_fields list))
  (let ((str (cl-call-next-method)))
    (concat str "\nKIND:individual")))

(cl-defmethod ebdb-fmt-record-body :around ((f ebdb-formatter-vcard-40)
					    (r ebdb-record-organization)
					    (_fields list))
  (let ((str (cl-call-next-method)))
    (concat str "\nKIND:org")))

(cl-defmethod ebdb-fmt-field ((f ebdb-formatter-vcard)
			      (field ebdb-field)
			      _style
			      _record)
  (ebdb-string field))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (field ebdb-field)
				    style
				    record)
  (upcase (cl-call-next-method f field style record)))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (field ebdb-field-timestamp)
				    _style
				    _record)
  "REV")

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (field ebdb-field-notes)
				    _style
				    _record)
  "NOTE")

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (field ebdb-field-creation-date)
				    _style
				    _record)
  "X-CREATION-DATE")

(cl-defmethod ebdb-fmt-field ((f ebdb-formatter-vcard)
			      (mail ebdb-field-mail)
			      _style
			      _record)
  (slot-value mail 'mail))
(cl-defmethod ebdb-fmt-field ((_f ebdb-formatter-vcard)
			      (ts ebdb-field-timestamp)
			      _style
			      _record)
  (format-time-string ":%Y%m%dT%H%M%S%z" (slot-value ts 'timestamp) t))

(cl-defmethod ebdb-fmt-field ((f ebdb-formatter-vcard)
			      (name ebdb-field-name-complex)
			      _style
			      _record)
  (with-slots (surname given-names prefix suffix) name
    (format
     "%s;%s;%s;%s"
     (or surname "")
     (if given-names (mapconcat #'identity given-names ","))
     (or prefix "")
     (or suffix ""))))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (field ebdb-field-uuid)
				    _style
				    _record)
  "UID:urn:uuid")

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (mail ebdb-field-mail)
				    _style
				    _record)
  (with-slots (priority) mail
    (format
     "EMAIL;INTERNET%s"
     (if (eql priority 'primary)
  	 ";PREF=1"
       ""))))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
			 	    (phone ebdb-field-phone)
				    _style
				    _record)
  (format "TEL;TYPE=%s" (slot-value phone 'object-name)))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (rel ebdb-field-relation)
				    _style
				    _record)
  (format "RELATED;TYPE=%s" (slot-value rel 'object-name)))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (url ebdb-field-url)
				    _style
				    _record)
  (format "URL;TYPE=%s" (slot-value url 'object-name)))

(cl-defmethod ebdb-fmt-field ((f ebdb-formatter-vcard)
			      (rel ebdb-field-relation)
			      _style
			      _record)
  (format "urn:uuid:%s" (slot-value rel 'rel-uuid)))

(cl-defmethod ebdb-fmt-field-label ((f ebdb-formatter-vcard)
				    (ann ebdb-field-anniversary)
				    _style
				    _record)
  (let* ((label (slot-value ann 'object-name))
	 (label-string
	  (if (string= label "birthday")
	      "BDAY"
	    (format "ANNIVERSARY;TYPE=%s" label))))
    (format "%s;CALSCALE=%s" label-string (slot-value ann 'calendar))))

(provide 'ebdb-vcard)
;;; ebdb-vcard.el ends here
