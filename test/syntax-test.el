;;; syntax-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-05-22 21:27:59 stm>

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

(message "Running Emacs %s with tests from %s"
         emacs-version (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

(declare-function puppet-test-with-temp-buffer (content &rest body))
(declare-function puppet-test-syntax-at (pos))


;;; Regular Expressions

(ert-deftest syntax/regular-expression-literal-match-op ()
  (puppet-test-with-temp-buffer "$foo =~ / class $foo/"
    (should (eq (puppet-test-syntax-at 9) 'punctuation))
    (should (eq (puppet-test-syntax-at 21) 'punctuation))))

(ert-deftest syntax/regular-expression-literal-no-match-op ()
  (puppet-test-with-temp-buffer "$foo !~ / class $foo/"
    (should (eq (puppet-test-syntax-at 9) 'punctuation))
    (should (eq (puppet-test-syntax-at 21) 'punctuation))))

(ert-deftest syntax/regular-expression-literal-node ()
  (puppet-test-with-temp-buffer "node / class $foo/ {}"
    (should (eq (puppet-test-syntax-at 6) 'punctuation))
    (should (eq (puppet-test-syntax-at 6) 'punctuation))))

(ert-deftest syntax/regular-expression-literal-selector ()
  (puppet-test-with-temp-buffer "/ class $foo/=>"
    (should (eq (puppet-test-syntax-at 1) 'punctuation))
    (should (eq (puppet-test-syntax-at 13) 'punctuation))))

(ert-deftest syntax/regular-expression-case ()
  (puppet-test-with-temp-buffer "/ class $foo/:"
    (should (eq (puppet-test-syntax-at 1) 'punctuation))
    (should (eq (puppet-test-syntax-at 13) 'punctuation))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax-test.el ends here
