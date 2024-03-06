;;; mode-setup-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-03-06 15:40:21 stm>

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
(require 'treesit)

;;; Tests
(ert-deftest puppet/treesitter-loaded ()
  :tags '(library)
  (should (featurep 'treesit)))

(ert-deftest puppet/treesitter-parser-loaded ()
  :tags '(library)
  (should (treesit-ready-p 'puppet)))

(ert-deftest puppet/puppet-ts-mode-loaded ()
  :tags '(library)
  (should (featurep 'puppet-ts-mode)))


(provide 'puppet-mode-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mode-setup-test.el ends here
