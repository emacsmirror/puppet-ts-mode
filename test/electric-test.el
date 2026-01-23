;;; electric-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2025, 2026 Stefan Möding

;; Author: Stefan Möding
;; Created: <2025-02-25 13:42:03 stm>
;; Updated: <2026-01-23 17:22:33 stm>

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
(declare-function puppet-ts-electric-greater (arg))


;;; Electric keys

(ert-deftest electric/resource_type ()
  (puppet-test-with-temp-buffer
      "
foo { 'foo':
  foobar => 1,
  foo
}"
  (goto-char (point-min))
  (end-of-line 4)
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "=>"))
  (insert "2,")
  (should (string= (buffer-string)
       "
foo { 'foo':
  foobar => 1,
  foo    => 2,
}"))))

(ert-deftest electric/resource_reference ()
  (puppet-test-with-temp-buffer
      "
Foo {
  foobar => 1,
  foo
}"
  (goto-char (point-min))
  (end-of-line 4)
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "=>"))
  (insert "2,")
  (should (string= (buffer-string)
       "
Foo {
  foobar => 1,
  foo    => 2,
}"))))

(ert-deftest electric/resource_collector ()
  (puppet-test-with-temp-buffer
      "
Foo <| |> {
  foobar => 1,
  foo
}"
  (goto-char (point-min))
  (end-of-line 4)
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "+>"))
  (insert "2,")
  (should (string= (buffer-string)
       "
Foo <| |> {
  foobar => 1,
  foo    +> 2,
}"))))

(ert-deftest electric/parameter ()
  (puppet-test-with-temp-buffer
      "
class foo (
  Integer $foobar = 1,
  String $foo
) {
}"
  (goto-char (point-min))
  (end-of-line 4)
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "="))
  (insert "2,")
  (should (string= (buffer-string)
       "
class foo (
  Integer $foobar = 1,
  String  $foo    = 2,
) {
}"))))

(ert-deftest electric/interpolation ()
  (puppet-test-with-temp-buffer
   "\"\"
"
  (goto-char (point-min))
  (forward-char)
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "$"))
  (should (string= (buffer-string)
   "\"${}\"
"))))

(ert-deftest electric/no-interpolation-1 ()
  (puppet-test-with-temp-buffer
   "
\"foo\"
"
  (goto-char (point-min))
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "$"))
  (should (string= (buffer-string)
   "$
\"foo\"
"))))

(ert-deftest electric/no-interpolation-2 ()
  (puppet-test-with-temp-buffer
   "\"foo\"
"
  (goto-char (point-min))
  (save-window-excursion
    (set-window-buffer nil (current-buffer))
    (execute-kbd-macro "$"))
  (should (string= (buffer-string)
   "$\"foo\"
"))))

;;; electric-test.el ends here
