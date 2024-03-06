;;; 00-mode-setup-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-03-06 13:11:14 stm>

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

(message "Running test in %s" (file-relative-name load-file-name))


;;; Requirements
(require 'ert)

;;; Tests
(ert-deftest puppet/feature-loaded ()
  :tags '(library)
  (should (featurep 'puppet-ts-mode)))

(provide 'puppet-mode-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 00-mode-setup-test.el ends here
