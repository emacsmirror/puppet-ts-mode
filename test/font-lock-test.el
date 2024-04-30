;;; font-lock-test.el --- Unit Test Suite  -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-04-30 08:14:34 stm>

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
(declare-function puppet-test-face-at (pos &optional content))


;;; Strings

(ert-deftest puppet/fontify-dq-string ()
  (should (eq (puppet-test-face-at 8 "$foo = \"bar\"") 'puppet-ts-string)))

(ert-deftest puppet/fontify-sq-string ()
  (should (eq (puppet-test-face-at 8 "$foo = 'bar'") 'puppet-ts-string)))

(ert-deftest puppet/escape-in-dq-string ()
  (puppet-test-with-temp-buffer "\"foo\\n\""
    (should (eq (puppet-test-face-at 1) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 5) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-string))))

;; Interpolation is not (yet) detected in a single quoted string

(ert-deftest puppet/variable-expansion-in-sq-string ()
  (puppet-test-with-temp-buffer "'${::foo::bar} yeah'"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 2) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 3) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 14) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 16) 'puppet-ts-string))))

(ert-deftest puppet/variable-expansion-in-dq-string ()
  (puppet-test-with-temp-buffer "\"${::foo::bar} yeah\""
    (should (eq (puppet-test-face-at 1) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 2) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 3) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 14) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 16) 'puppet-ts-string))))


;;; Numbers

(ert-deftest puppet/number-integer ()
  (puppet-test-with-temp-buffer "$x = 42"
    (should (eq (puppet-test-face-at 6) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-number))))

(ert-deftest puppet/number-float ()
  (puppet-test-with-temp-buffer "$x = 4.2"
    (should (eq (puppet-test-face-at 6) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 8) 'puppet-ts-number))))

(ert-deftest puppet/number-scientific ()
  (puppet-test-with-temp-buffer "$x = 4.2e12"
    (should (eq (puppet-test-face-at 6) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 9) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 11) 'puppet-ts-number))))

(ert-deftest puppet/number-hex ()
  (puppet-test-with-temp-buffer "$x = 0x42"
    (should (eq (puppet-test-face-at 6) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-number))
    (should (eq (puppet-test-face-at 9) 'puppet-ts-number))))

;;; Operators

(ert-deftest puppet/operator-negation ()
  (puppet-test-with-temp-buffer "$x = !true"
    (should (eq (puppet-test-face-at 6) 'puppet-ts-negation-char))))

(ert-deftest puppet/operator-compare ()
  (puppet-test-with-temp-buffer "$x > $y"
    (should (eq (puppet-test-face-at 4) 'puppet-ts-operator))))


;;; Comments

(ert-deftest puppet/fontify-line-comment ()
  (puppet-test-with-temp-buffer "# class
bar"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-comment))
    (should (eq (puppet-test-face-at 3) 'puppet-ts-comment))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-comment))
    (should-not (puppet-test-face-at 8))
    (should-not (puppet-test-face-at 9))))

;; While C-style comments were documented for Puppet 5.5, they are no longer
;; documented as supported for Puppet 7 and 8.
;;
;; (ert-deftest puppet/fontify-c-style-comment ()
;;   (puppet-test-with-temp-buffer "/*
;; class */ bar"
;;     (should (eq (puppet-test-face-at 1) 'puppet-ts-comment))
;;     (should (eq (puppet-test-face-at 4) 'puppet-ts-comment))
;;     (should (eq (puppet-test-face-at 8) 'puppet-ts-comment))
;;     (should (eq (puppet-test-face-at 11) 'puppet-ts-comment))
;;     (should-not (puppet-test-face-at 13))))


;;; Regular Expressions

;; (ert-deftest puppet-font-lock-keywords/regular-expression-literal-match-op ()
;;   (puppet-test-with-temp-buffer "$foo =~ / class $foo/ {"
;;     (should (eq (puppet-test-face-at 9) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 11) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 17) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 21) 'puppet-regular-expression-literal))
;;     (should-not (puppet-test-face-at 23))))

;; (ert-deftest puppet-font-lock-keywords/regular-expression-literal-no-match-op ()
;;   (puppet-test-with-temp-buffer "$foo !~ / class $foo/ {"
;;     (should (eq (puppet-test-face-at 9) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 11) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 17) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 21) 'puppet-regular-expression-literal))
;;     (should-not (puppet-test-face-at 23))))

;; (ert-deftest puppet-font-lock-keywords/regular-expression-literal-node ()
;;   (puppet-test-with-temp-buffer "node / class $foo/ {"
;;     (should (eq (puppet-test-face-at 6) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 8) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 14) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 18) 'puppet-regular-expression-literal))
;;     (should-not (puppet-test-face-at 20))))

;; (ert-deftest puppet-font-lock-keywords/regular-expression-literal-selector ()
;;   (puppet-test-with-temp-buffer "/ class $foo/=>"
;;     (should (eq (puppet-test-face-at 1) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 3) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 9) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 13) 'puppet-regular-expression-literal))
;;     (should-not (puppet-test-face-at 14))))

;; (ert-deftest puppet-font-lock-keywords/regular-expression-case ()
;;   (puppet-test-with-temp-buffer "/ class $foo/:"
;;     (should (eq (puppet-test-face-at 1) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 3) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 9) 'puppet-regular-expression-literal))
;;     (should (eq (puppet-test-face-at 13) 'puppet-regular-expression-literal))
;;     (should-not (puppet-test-face-at 14))))

;; (ert-deftest puppet/invalid-regular-expression ()
;;   (puppet-test-with-temp-buffer "$foo = / class $foo/"
;;     (should-not (puppet-test-face-at 8))
;;     (should (eq (puppet-test-face-at 10) 'puppet-ts-keyword))
;;     (should (eq (puppet-test-face-at 16) 'puppet-ts-variable-name))
;;     (should-not (puppet-test-face-at 20))))


;;; Keywords

(ert-deftest puppet/keyword-in-symbol ()
  (should (eq (puppet-test-face-at 7 "class fooclass { }") 'puppet-ts-resource-type)))

(ert-deftest puppet/keyword-in-parameter-name ()
  (should-not (puppet-test-face-at 16 "class { 'foo': node_foo => bar }")))

(ert-deftest puppet/keyword-as-parameter-name ()
  (should-not (puppet-test-face-at 16 "class { 'foo': unless => bar }")))

(ert-deftest puppet/simple-variable ()
  (puppet-test-with-temp-buffer "$foo = 5"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-variable-name))
    ;; The operator should not get highlighted
    (should-not (puppet-test-face-at 6))))

(ert-deftest puppet/simple-variable-no-space ()
  (puppet-test-with-temp-buffer "$foo=5"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-variable-name))
    ;; The operator should not get highlighted
    (should-not (puppet-test-face-at 5))))

(ert-deftest puppet/variable-with-scope ()
  (puppet-test-with-temp-buffer "$foo::bar = 5"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 2) 'puppet-ts-variable-name))
    ;; Scope operator
    (should (eq (puppet-test-face-at 5) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-variable-name))))

(ert-deftest puppet/variable-in-top-scope ()
  (puppet-test-with-temp-buffer "$::foo = 5"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 2) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-variable-name))))

(ert-deftest puppet/variable-before-colon ()
  (puppet-test-with-temp-buffer "package { $::foo: }"
    (should (eq (puppet-test-face-at 11) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 12) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 14) 'puppet-ts-variable-name))
    (should (eq (puppet-test-face-at 16) 'puppet-ts-variable-name))
    (should-not (puppet-test-face-at 17))))

(ert-deftest puppet/class ()
  (puppet-test-with-temp-buffer "class foo::bar { }"
    ;; The keyword
    (should (eq (puppet-test-face-at 1) 'puppet-ts-keyword))
    ;; The scope
    (should (eq (puppet-test-face-at 7) 'puppet-ts-resource-type))
    ;; The scope operator
    (should (eq (puppet-test-face-at 10) 'puppet-ts-resource-type))
    ;; The name
    (should (eq (puppet-test-face-at 12) 'puppet-ts-resource-type))
    ;; The braces
    (should-not (puppet-test-face-at 16))))

(ert-deftest puppet/define ()
  (puppet-test-with-temp-buffer "define foo::bar($foo) { }"
    ;; The keyword
    (should (eq (puppet-test-face-at 1) 'puppet-ts-keyword))
    ;; The scope
    (should (eq (puppet-test-face-at 8) 'puppet-ts-resource-type))
    ;; The scope operator
    (should (eq (puppet-test-face-at 11) 'puppet-ts-resource-type))
    ;; The name
    (should (eq (puppet-test-face-at 13) 'puppet-ts-resource-type))
    ;; The parenthesis
    (should-not (puppet-test-face-at 16))
    ;; The parameter
    (should (eq (puppet-test-face-at 17) 'puppet-ts-variable-name))
    (should-not (puppet-test-face-at 21))))

(ert-deftest puppet/node ()
  (puppet-test-with-temp-buffer "node puppet.example.com { }"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-keyword))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 12) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 23) 'puppet-ts-resource-type))
    (should-not (puppet-test-face-at 25))))

(ert-deftest puppet/plan ()
  (puppet-test-with-temp-buffer "plan foo::bar($foo) { }"
    ;; The keyword
    (should (eq (puppet-test-face-at 1) 'puppet-ts-keyword))
    ;; The scope
    (should (eq (puppet-test-face-at 6) 'puppet-ts-resource-type))
    ;; The scope operator
    (should (eq (puppet-test-face-at 9) 'puppet-ts-resource-type))
    ;; The name
    (should (eq (puppet-test-face-at 11) 'puppet-ts-resource-type))
    ;; The parenthesis
    (should-not (puppet-test-face-at 14))
    ;; The parameter
    (should (eq (puppet-test-face-at 15) 'puppet-ts-variable-name))
    (should-not (puppet-test-face-at 19))))

(ert-deftest puppet/resource ()
  (puppet-test-with-temp-buffer "foo::bar { 'title': }"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-resource-type))
    (should-not (puppet-test-face-at 10))))

(ert-deftest puppet/resource-default ()
  (puppet-test-with-temp-buffer "Foo::Bar { }"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-resource-type))
    (should-not (puppet-test-face-at 10))))

(ert-deftest puppet/resource-default-not-capitalized ()
  (puppet-test-with-temp-buffer "Foo::bar { }"
    (should-not (puppet-test-face-at 6))
    (should-not (puppet-test-face-at 8))))

(ert-deftest puppet/resource-collector ()
  (puppet-test-with-temp-buffer "Foo::Bar <||>"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-resource-type))
    (should-not (puppet-test-face-at 10))))

(ert-deftest puppet/exported-resource-collector ()
  (puppet-test-with-temp-buffer "Foo::Bar <<| |>>}"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 4) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 6) 'puppet-ts-resource-type))
    (should-not (puppet-test-face-at 10))
    (should-not (puppet-test-face-at 11))))

(ert-deftest puppet/resource-collector-not-capitalized ()
  (puppet-test-with-temp-buffer "Foo::bar <<| |>>"
    (should-not (puppet-test-face-at 6))
    (should-not (puppet-test-face-at 10))
    (should-not (puppet-test-face-at 11))))

(ert-deftest puppet/negation ()
  (should (eq (puppet-test-face-at 8 "$foo = !$bar") 'puppet-ts-negation-char)))

(ert-deftest puppet/builtin-metaparameter ()
  (puppet-test-with-temp-buffer "class { 'foo': alias => foo }"
    (should (eq (puppet-test-face-at 16) 'puppet-ts-builtin))
    (should-not (puppet-test-face-at 22))))

(ert-deftest puppet/builtin-metaparameter-no-space ()
  (puppet-test-with-temp-buffer "class { 'foo': alias=>foo }"
    (should (eq (puppet-test-face-at 16) 'puppet-ts-builtin))
    (should-not (puppet-test-face-at 21))))

(ert-deftest puppet/builtin-function ()
  (puppet-test-with-temp-buffer "template('foo/bar')"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-builtin))
    (should-not (puppet-test-face-at 9))
    (should (eq (puppet-test-face-at 10) 'puppet-ts-string))))

(ert-deftest puppet/builtin-function-in-parameter-name ()
  (puppet-test-with-temp-buffer "package { 'foo': ensure => installed }"
    (should (eq (puppet-test-face-at 18) 'puppet-ts-builtin))
    (should-not (puppet-test-face-at 24))))

(ert-deftest puppet/type-argument-to-contain ()
  (puppet-test-with-temp-buffer "contain foo::bar"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 9) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 12) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 14) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 16) 'puppet-ts-resource-type))))

(ert-deftest puppet/string-argument-to-contain ()
  (puppet-test-with-temp-buffer "contain 'foo::bar'"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 9) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 10) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 13) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 15) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 17) 'puppet-ts-string))
    (should (eq (puppet-test-face-at 18) 'puppet-ts-string))))

(ert-deftest puppet/type-argument-to-include ()
  (puppet-test-with-temp-buffer "include foo::bar"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 9) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 12) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 14) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 16) 'puppet-ts-resource-type))))

(ert-deftest puppet/type-argument-to-require ()
  (puppet-test-with-temp-buffer "require foo::bar"
    (should (eq (puppet-test-face-at 1) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 7) 'puppet-ts-builtin))
    (should (eq (puppet-test-face-at 9) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 12) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 14) 'puppet-ts-resource-type))
    (should (eq (puppet-test-face-at 16) 'puppet-ts-resource-type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; font-lock-test.el ends here
