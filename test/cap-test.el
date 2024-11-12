;;; cap-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-11-12 13:05:03 stm>
;; Updated: <2024-11-12 15:29:29 stm>

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

(message "Running Emacs %s with tests from %s"
         emacs-version (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

(declare-function puppet-test-with-temp-buffer (content &rest body))


;;;; Completion at point

(ert-deftest completion/unique-variable ()
  (puppet-test-with-temp-buffer
   "
class foo (
  String $ensure  = 'present',
  String $version = '42',
) {
  $v
}"
   (goto-char (point-min))
   (end-of-line 6)
   (completion-at-point)
   (should (looking-back "^ *$version$"))))

(ert-deftest completion/unique-prefix-variable ()
  (puppet-test-with-temp-buffer
   "
class foo (
  String  $ensure,
  Boolean $enable,
) {
  $ens
}"
   (goto-char (point-min))
   (end-of-line 6)
   (completion-at-point)
   (should (looking-back "^ *$ensure$"))))

(ert-deftest completion/facts-variable ()
  (puppet-test-with-temp-buffer
   "
class foo (
  String  $ensure,
  Boolean $enable,
) {
  $f
}"
   (goto-char (point-min))
   (end-of-line 6)
   (completion-at-point)
   (should (looking-back "^ *$facts$"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cap-test.el ends here
