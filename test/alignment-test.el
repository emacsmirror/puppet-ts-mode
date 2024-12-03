;;; alignment-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-12-03 10:12:27 stm>

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
(declare-function puppet-ts-align-block ())



(ert-deftest align/one-block ()
  (puppet-test-with-temp-buffer
      "
package { 'foo':
  ensure => latest,
  require    => Package['bar'],
  install_options =>   ['--foo', '--bar']
}"
      (search-forward "'foo':")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
package { 'foo':
  ensure          => latest,
  require         => Package['bar'],
  install_options => ['--foo', '--bar']
}"))))

(ert-deftest align/resource-collector ()
  (puppet-test-with-temp-buffer
      "
Foo <<| tag == |>> {
  ensure => latest,
  require    => Package['bar'],
  install_options =>   ['--foo', '--bar']
}"
      (search-forward "bar")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
Foo <<| tag == |>> {
  ensure          => latest,
  require         => Package['bar'],
  install_options => ['--foo', '--bar']
}"))))

(ert-deftest align/selector ()
  (puppet-test-with-temp-buffer
      "
$rootgroup = $facts['os']['family'] ? {
  'RedHat'                 => 'wheel',
  /(Debian|Ubuntu)/  => 'wheel',
  default =>    'root',
}"
      (search-forward "wheel")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
$rootgroup = $facts['os']['family'] ? {
  'RedHat'          => 'wheel',
  /(Debian|Ubuntu)/ => 'wheel',
  default           => 'root',
}"))))

(ert-deftest align/resource-collector-add ()
  (puppet-test-with-temp-buffer
      "
Foo <<| tag == |>> {
  ensure => latest,
  require    => Package['bar'],
  install_options +>   ['--foo', '--bar']
}"
      (search-forward "bar")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
Foo <<| tag == |>> {
  ensure          => latest,
  require         => Package['bar'],
  install_options +> ['--foo', '--bar']
}"))))

(ert-deftest align/stays-within-one-block ()
  (puppet-test-with-temp-buffer
      "
package { 'foo':
  ensure => latest,
  require    => Package['bar'],
  install_options =>   ['--foo', '--bar']
}
package { 'bar':
  ensure    => latest,
  install_options => [],
}"
      (search-forward "'foo':")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
package { 'foo':
  ensure          => latest,
  require         => Package['bar'],
  install_options => ['--foo', '--bar']
}
package { 'bar':
  ensure    => latest,
  install_options => [],
}"))))

(ert-deftest align/skip-nested-blocks ()
  (puppet-test-with-temp-buffer
      "
package { 'foo':
  ensure => latest,
  require    => Package['bar'],
  install_options =>   ['--foo', '--bar'],
  foo => {
    bar => 'qux',
    quxc => 'bar',
  }
}"
      (search-forward "'foo':")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
package { 'foo':
  ensure          => latest,
  require         => Package['bar'],
  install_options => ['--foo', '--bar'],
  foo             => {
    bar => 'qux',
    quxc => 'bar',
  }
}"))))

(ert-deftest align/ignores-commented-out-lines ()
  (puppet-test-with-temp-buffer
      "
package { 'foo':
  ensure  => latest,
  # require => Package['bar'],
}"
      (search-forward "'foo':")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
package { 'foo':
  ensure => latest,
  # require => Package['bar'],
}"))))

(ert-deftest align/skip-previous-nested-block ()
  (puppet-test-with-temp-buffer
      "
class foo {
  $x = {
    'a'=>1,
    'foo'=>{
      'apples'=>1,
    },
    'metafalica'=>1,
  }
}"
      (search-forward "'metafalica'")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo {
  $x = {
    'a'          => 1,
    'foo'        => {
      'apples'=>1,
    },
    'metafalica' => 1,
  }
}"))))

(ert-deftest align/title-variable ()
  (puppet-test-with-temp-buffer
      "
file { $foo:
  foo => $foo,
  foobar => $foobar,
}"
      (search-forward "foo:")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
file { $foo:
  foo    => $foo,
  foobar => $foobar,
}"))))

(ert-deftest align/point-in-string ()
  (puppet-test-with-temp-buffer
      "
class foo {
  $x = {
    'a'=>1,
    'foo'=>{
      'apples'=>1,
    },
    'metafalica'=>1,
  }
}"
      (search-forward "tafalica")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo {
  $x = {
    'a'          => 1,
    'foo'        => {
      'apples'=>1,
    },
    'metafalica' => 1,
  }
}"))))

;;; Alignment tests for parameter list

(ert-deftest align/no-block ()
  (puppet-test-with-temp-buffer
      "
class foo (String    $foo) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (String $foo) {
}"))))

(ert-deftest align/block-1-param ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String $foo,
) {
}"))))

(ert-deftest align/block-2-param ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo,
  Boolean    $bar,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String  $foo,
  Boolean $bar,
) {
}"))))

(ert-deftest align/block-3-param ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo,
  Boolean    $bar,
  Integer $foobar,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String  $foo,
  Boolean $bar,
  Integer $foobar,
) {
}"))))

(ert-deftest align/block-nested ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo,
  Optional[Integer,Enum['bar','baz']]    $bar,
  Integer $foobar,
) {
}"
      (search-forward "baz")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String                              $foo,
  Optional[Integer,Enum['bar','baz']] $bar,
  Integer                             $foobar,
) {
}"))))

(ert-deftest align/block-1-default ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo   = 'foo',
  Boolean    $bar,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String  $foo = 'foo',
  Boolean $bar,
) {
}"))))

(ert-deftest align/block-2-default ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo   = 'foo',
  Boolean    $bar =   true,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String  $foo = 'foo',
  Boolean $bar = true,
) {
}"))))

(ert-deftest align/block-3-default ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo   = 'foo',
  Boolean    $bar =   true,
  Integer $foobar    =   42,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String  $foo    = 'foo',
  Boolean $bar    = true,
  Integer $foobar = 42,
) {
}"))))

(ert-deftest align/block-3-mixed ()
  (puppet-test-with-temp-buffer
      "
class foo (
  String    $foo,
  Boolean    $bar =   true,
  Integer $foobar    =   42,
) {
}"
      (search-forward "String")
      (puppet-ts-align-block)
      (should (string= (buffer-string) "
class foo (
  String  $foo,
  Boolean $bar    = true,
  Integer $foobar = 42,
) {
}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alignment-test.el ends here
