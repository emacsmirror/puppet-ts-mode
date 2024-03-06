;;; 10-font-lock-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-03-06 15:35:04 stm>

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

;;; Code:

(message "Running tests in %s" (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

(declare-function puppet-test-face-at "test-helper.el" (pos &optional content))
(declare-function puppet-test-with-temp-buffer (content &rest body))

;;; Tests
(ert-deftest puppet/fontify-dq-string ()
  :tags '(fontification)
  (should (eq (puppet-test-face-at 8 "$foo = \"bar\"") 'puppet-string-face)))

(ert-deftest puppet/fontify-sq-string ()
  :tags '(fontification)
  (should (eq (puppet-test-face-at 8 "$foo = 'bar'") 'puppet-string-face)))

(ert-deftest puppet/fontify-line-comment ()
  :tags '(fontification syntax-table)
  (puppet-test-with-temp-buffer "# class
bar"
    (should (eq (puppet-test-face-at 1) 'puppet-comment-face))
    (should (eq (puppet-test-face-at 3) 'puppet-comment-face))
    (should (eq (puppet-test-face-at 7) 'puppet-comment-face))
    (should (eq (puppet-test-face-at 8) 'puppet-comment-face))
    (should-not (puppet-test-face-at 9))))

(ert-deftest puppet/fontify-c-style-comment ()
  :tags '(fontification syntax-table)
  (puppet-test-with-temp-buffer "/*
class */ bar"
    (should (eq (puppet-test-face-at 1) 'puppet-comment-face))
    (should (eq (puppet-test-face-at 4) 'puppet-comment-face))
    (should (eq (puppet-test-face-at 8) 'puppet-comment-face))
    (should (eq (puppet-test-face-at 11) 'puppet-comment-face))
    (should-not (puppet-test-face-at 13))))

(provide 'puppet-mode-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10-font-lock-test.el ends here
