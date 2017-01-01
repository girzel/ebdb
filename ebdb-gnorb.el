;;; ebdb-gnorb.el --- Utilities for connecting EBDB to Gnorb  -*- lexical-binding: t; -*-

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

;; Bits and pieces useful for tying EBDB in with Gnorb.  Everything in
;; this file can be moved elsewhere.

;;; Code:

(cl-defstruct gnorb-ebdb-link
  subject date group id)

(defclass gnorb-ebdb-field-messages (ebdb-field-user)
  ((messages
    :type (list-of gnorb-ebdb-link)
    :initarg :messages
    :initform nil))
  :human-readable "gnorb messages")

(cl-defmethod ebdb-string ((_field gnorb-ebdb-field-messages))
  "Some messages")

(provide 'ebdb-gnorb)
