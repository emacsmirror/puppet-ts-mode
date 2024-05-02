;;; quotes-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-05-02 15:58:30 stm>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit test suite for puppet-ts-mode

;; Most test cases are taken from the original puppet-mode.

;;; Code:

(message "Booting Emacs %s and run tests from %s"
         emacs-version (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

(declare-function puppet-test-with-temp-buffer (content &rest body))
(declare-function cycle-quotes ())


;;; Single quotes

(ert-deftest puppet/cycle-single-quotes ()
  (puppet-test-with-temp-buffer
   "$x = 'foo'"
   (goto-char 7)
   (cycle-quotes)
   (should (string= (buffer-string)
                    "$x = \"foo\""))))

(ert-deftest puppet/cycle-double-quotes ()
  (puppet-test-with-temp-buffer
   "$x = \"foo\""
   (goto-char 7)
   (cycle-quotes)
   (should (string= (buffer-string)
                    "$x = 'foo'"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; quotes-test.el ends here
