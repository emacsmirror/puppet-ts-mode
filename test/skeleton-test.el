;;; skeleton-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-05-22 21:13:42 stm>

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
(declare-function puppet-ts-filename-parser (file))
(declare-function puppet-ts-keyword-class ())
(declare-function puppet-ts-keyword-define ())
(declare-function puppet-ts-keyword-node ())
(declare-function puppet-ts-type-anchor ())
(declare-function puppet-ts-type-class ())
(declare-function puppet-ts-type-exec ())
(declare-function puppet-ts-type-file ())
(declare-function puppet-ts-type-group ())
(declare-function puppet-ts-type-host ())
(declare-function puppet-ts-type-notify ())
(declare-function puppet-ts-type-package ())
(declare-function puppet-ts-type-service ())
(declare-function puppet-ts-type-user ())


;;;; Skeletons

(ert-deftest skeleton/puppet-ts-filename-parser-unix ()
  (should (equal (puppet-ts-filename-parser "/modules/foo/manifests/init.pp")
                 '("foo")))
  (should (equal (puppet-ts-filename-parser "/modules/foo/manifests/bar.pp")
                 '("foo" "bar")))
  (should (equal (puppet-ts-filename-parser "/modules/foo/manifests/bar/baz.pp")
                 '("foo" "bar" "baz"))))

(ert-deftest skeleton/puppet-ts-filename-parser-windows ()
  (should (equal (puppet-ts-filename-parser "C:/modules/foo/manifests/init.pp")
                 '("foo")))
  (should (equal (puppet-ts-filename-parser "C:/modules/foo/manifests/bar.pp")
                 '("foo" "bar")))
  (should (equal (puppet-ts-filename-parser "C:/modules/foo/manifests/bar/baz.pp")
                 '("foo" "bar" "baz"))))

(ert-deftest skeleton/puppet-ts-filename-parser-unidentified ()
  (should (equal (puppet-ts-filename-parser "/modules/init.pp")
                 '("unidentified")))
  (should (equal (puppet-ts-filename-parser "/modules/foo/init.pp")
                 '("unidentified")))
  (should (equal (puppet-ts-filename-parser "/modules/foo/bar/init.pp")
                 '("unidentified"))))

(ert-deftest skeleton/puppet-ts-filename-parser-nil ()
  (should (equal (puppet-ts-filename-parser nil)
                 '("unidentified"))))

(ert-deftest skeleton/puppet-ts-filename-parser-invalid-module ()
  (should (equal (puppet-ts-filename-parser "/puppet-foo/manifests/init.pp")
                 '("foo"))))

(ert-deftest skeleton/keyword-class ()
  (puppet-test-with-temp-buffer
   ""
   (set-visited-file-name "/modules/foo/manifests/bar.pp" t)
   (puppet-ts-keyword-class)
   (should (string= (buffer-string) "class foo::bar (
) {
}"))))

(ert-deftest skeleton/keyword-define ()
  (puppet-test-with-temp-buffer
   ""
   (set-visited-file-name "/modules/foo/manifests/bar.pp" t)
   (puppet-ts-keyword-define)
   (should (string= (buffer-string) "define foo::bar (
) {
}"))))

(ert-deftest skeleton/keyword-node ()
  (puppet-test-with-temp-buffer
   ""
   (puppet-ts-keyword-node)
   (should (looking-at " {"))           ; node name
   (should (string= (buffer-string) "node  {
}"))))

(ert-deftest skeleton/type-anchor ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-anchor)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  anchor { : }
}"))))

(ert-deftest skeleton/type-class ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-class)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  class { :
  }
}"))))

(ert-deftest skeleton/type-exec ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-exec)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  exec { :
    path => [ '/bin', '/sbin', '/usr/bin', '/usr/sbin', ],
    user => 'root',
    cwd  => '/',
  }
}"))))

(ert-deftest skeleton/type-file ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-file)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  file { :
    ensure => file,
    owner  => 'root',
    group  => 'root',
    mode   => '0644',
  }
}"))))

(ert-deftest skeleton/type-group ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-group)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  group { :
    ensure => present,
  }
}"))))

(ert-deftest skeleton/type-host ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-host)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  host { :
    ensure => present,
  }
}"))))

(ert-deftest skeleton/type-notify ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-notify)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  notify { : }
}"))))

(ert-deftest skeleton/type-package ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-package)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  package { :
    ensure => present,
  }
}"))))

(ert-deftest skeleton/type-service ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-service)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  service { :
    ensure => running,
    enable => true,
  }
}"))))

(ert-deftest skeleton/type-user ()
  (puppet-test-with-temp-buffer
   "
class foo {

}"
   (goto-char (point-min))
   (forward-line 2)
   (puppet-ts-type-user)
   (should (looking-at ":"))            ; title
   (should (string= (buffer-string) "
class foo {
  user { :
    ensure   => present,
    shell    => '/bin/bash',
    password => '*',
  }
}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; skeleton-test.el ends here
