;;; ebdb-test.el --- Tests for EBDB                  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for EBDB.

;;; Code:

(require 'ert)
(require 'ebdb)
(require 'ebdb-snarf)

;; Testing macros.

(defmacro ebdb-test-with-database (db-and-filename &rest body)
  "Macro providing a temporary database to work with."
  (declare (indent 1) (debug t))
  `(ebdb-test-save-vars
     (let ((,(car db-and-filename) (make-instance 'ebdb-db-file
						  :file ,(nth 1 db-and-filename)
						  :dirty t)))
       (ebdb-db-save ,(car db-and-filename))
       (unwind-protect
	   (progn
	     ,@body)
	 (delete-file ,(nth 1 db-and-filename))))))

(defmacro ebdb-test-save-vars (&rest body)
  "Don't let EBDB tests pollute `ebdb-record-tracker' and
`ebdb-db-list'."
  (declare (indent 0) (debug t))
  (let ((old-record-tracker (cl-gensym))
	(old-db-list (cl-gensym)))
    `(let ((,old-record-tracker ebdb-record-tracker)
	   (,old-db-list ebdb-db-list)
	   (ebdb-record-tracker nil)
	   (ebdb-db-list nil))
       (unwind-protect
	   (progn
	     ,@body)
	 (setq ebdb-record-tracker ,old-record-tracker
	       ebdb-db-list ,old-db-list)))))

;; Test database file name.
(defvar ebdb-test-database-1 (make-temp-name
			      (expand-file-name
			       "ebdb-test-db-1-"
			       temporary-file-directory)))

(ert-deftest ebdb-make-database ()
  "Make a database and save it to disk."
  (ebdb-test-with-database (db ebdb-test-database-1)
    (should (file-exists-p ebdb-test-database-1))
    (should (null (slot-value db 'dirty)))))

(ert-deftest ebdb-read-database ()
  "Read a database from file."
  (ebdb-test-with-database (db ebdb-test-database-1)
    (let ((reloaded
	   (eieio-persistent-read ebdb-test-database-1 'ebdb-db t)))
      (should (object-of-class-p reloaded 'ebdb-db-file)))))

(ert-deftest ebdb-database-unsynced ()
  "Make sure database knows it's unsynced."
  (ebdb-test-with-database (db ebdb-test-database-1)
    ;; Sync-time doesn't get updated until we load it.
    (ebdb-db-load db)
    ;; Apparently the two calls are too close together to register a
    ;; difference in time, which I find weird.
    (sit-for 0.1)
    (append-to-file "\n;; Junk string" nil (slot-value db 'file))
    (should (ebdb-db-unsynced db))))

(ert-deftest ebdb-make-record ()
  (ebdb-test-save-vars
   (let ((rec (make-instance ebdb-default-record-class)))
     (should (object-of-class-p rec 'ebdb-record)))))

(ert-deftest ebdb-add-record ()
  "Create a record, add it to DB, and make sure it has a UUID."
  (ebdb-test-save-vars
    (ebdb-test-with-database (db ebdb-test-database-1)
      (let ((rec (make-instance 'ebdb-record-person)))
	(should (null (ebdb-record-uuid rec)))
	(ebdb-db-add-record db rec)
	(should (stringp (ebdb-record-uuid rec)))))))

;; Very basic sanity tests for field instances.

(ert-deftest ebdb-parse-mail-and-name ()
  "Go through some basic field classes and ensure that a string
  run through `ebdb-parse' and `ebdb-string' remains the same."
  (let ((pairs
	 '((ebdb-field-mail "eric@ericabrahamsen.net")
	   (ebdb-field-mail "Eric Abrahamsen <eric@ericabrahamsen.net>")
	   (ebdb-field-name "Eric Abrahamsen")
	   (ebdb-field-name "Eric P. Abrahamsen, III"))))
    (dolist (p pairs)
      (should (equal (ebdb-string (ebdb-parse (car p) (nth 1 p)))
		     (nth 1 p))))))

;; Snarf testing.

(ert-deftest ebdb-snarf-mail-and-name ()
  (let ((test-texts
	 '("Eric Abrahamsen <eric@ericabrahamsen.net>"
	   "Eric Abrahamsen eric@ericabrahamsen.net"
	   "Eric Abrahamsen \n <eric@ericabrahamsen.net>"
	   "Eric Abrahamsen can't hold his drink\n<eric@ericabrahamsen.net> is where you can write and tell him so."))
	result)
    (dolist (text test-texts)
      (with-temp-buffer
	(insert text)
	(setq result (car (ebdb-snarf-collect)))
	(pcase result
	  (`[nil (,name) (,mail)]
	   (unless (string= (ebdb-string name) "Eric Abrahamsen")
	     (ert-fail (list (format "Parsing \"%s\" resulted in name %s"
				     text (ebdb-string name)))))
	   (unless (string= (ebdb-string mail) "eric@ericabrahamsen.net")
	     (ert-fail (list (format "Parsing \"%s\" resulted in mail %s"
				     text (ebdb-string mail))))))
	  (_ (ert-fail (list (format "Parsing \"%s\" resulted in %s"
				     text result)))))))))

(provide 'ebdb-test)
;;; ebdb-test.el ends here
