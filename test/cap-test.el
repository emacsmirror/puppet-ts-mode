;;; cap-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024, 2025 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-11-12 13:05:03 stm>
;; Updated: <2025-01-07 17:10:41 stm>

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
   (should (looking-back "  $version" (pos-bol)))))

(ert-deftest completion/scoped-variable ()
  (puppet-test-with-temp-buffer
   "
$foo::bar = 1
$fo
"
   (goto-char (point-min))
   (end-of-line 3)
   (completion-at-point)
   (should (looking-back "foo::bar" (pos-bol)))))

(ert-deftest completion/interpolated-variable ()
  (puppet-test-with-temp-buffer
   "
class foo (
  String $ensure  = 'present',
  String $version = '42',
) {
  \"${v}\"
}"
   (goto-char (point-min))
   (end-of-line 6)
   (backward-char 2)
   (completion-at-point)
   (should (and (looking-back "  \"${version" (pos-bol))
                (looking-at "}\"")))))

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
   (should (looking-back "  $ensure" (pos-bol)))))

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
   (should (looking-back "  $facts" (pos-bol)))))

(ert-deftest completion/trailing-colon ()
  (puppet-test-with-temp-buffer
   "
class foo (
  String  $package_name,
) {
  class { $p:
  }
}"
   (goto-char (point-min))
   (end-of-line 5)
   (backward-char)
   (completion-at-point)
   (should (looking-back "  class { $package_name" (pos-bol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cap-test.el ends here
