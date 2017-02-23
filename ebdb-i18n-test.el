;;; ebdb-i18n-test.el --- Tests for EBDB's internationalization support  -*- lexical-binding: t; -*-

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

;; This file obviously depends on ebdb-i18n and all region-specific
;; files that come with EBDB, and if you run the tests in an
;; interactive session it will load those files.  If you had EBDB
;; running as a normal user, loading these tests might change EBDB's
;; behavior.

;;; Code:

(require 'ert)
(require 'ebdb-i18n)
(require 'ebdb-chn)

;; Basic name parsing.

;; Regular latin names shouldn't be parsed any differently with the
;; i18n files loaded.

(ert-deftest ebdb-i18n-parse-name ()
  (let ((max (ebdb-parse 'ebdb-field-name-complex "Max von Sydow"))
	(brigitte (ebdb-parse 'ebdb-field-name-complex "Brigitte Bardot")))
    (should (string= (ebdb-name-last max) "von Sydow"))
    (should (string= (ebdb-name-last brigitte) "Bardot"))))

(ert-deftest ebdb-i18n-parse-chinese-name ()
  "Parse names in Chinese.

Uses `ebdb-parse-i18n' method from ebdb-chn.el."
  (let ((two-char (ebdb-parse 'ebdb-field-name-complex "李四"))
	(three-char (ebdb-parse 'ebdb-field-name-complex "张国荣"))
	(compound-surname-1 (ebdb-parse 'ebdb-field-name-complex "司马迁"))
	(compound-surname-2 (ebdb-parse 'ebdb-field-name-complex "慕容学村")))
    (should (string= (ebdb-name-last two-char) "李"))
    (should (string= (ebdb-name-last three-char) "张"))
    (should (string= (ebdb-name-last compound-surname-1) "司马"))
    (should (string= (ebdb-name-last compound-surname-2) "慕容"))))

(provide 'ebdb-i18n-test)
;;; ebdb-i18n-test.el ends here
