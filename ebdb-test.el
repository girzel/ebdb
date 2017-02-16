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
(require 'pcase)
(require 'ebdb)
(require 'ebdb-snarf)

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
