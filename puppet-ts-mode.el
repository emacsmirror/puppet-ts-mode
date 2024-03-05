;;; puppet-ts-mode.el --- Major mode for editing Puppet files using tree-sitter -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author:           Stefan Möding
;; Version:          0.1
;; Created:          <2024-03-02 13:05:03 stm>
;; Updated:          <2024-03-05 16:48:00 stm>
;; Keywords:         Puppet Treesitter
;; Package-Requires: ((emacs "29.1"))
;; Homepage:         https://github.com/smoeding/puppet-ts-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This is puppet-ts-mode, a major mode to edit Puppet files using the
;; tree-sitter parser for Puppet.  You can install the parser using the
;; following Elisp snippet:
;;
;; (add-to-list
;;  'treesit-language-source-alist
;;  '(puppet "https://github.com/tree-sitter-grammars/tree-sitter-puppet"))
;; (treesit-install-language-grammar 'puppet)
;;
;; Also consult the Emacs manual: (info "(elisp) Parsing Program Source")
;;
;; Caution: This is work in progress; many details concerning font-lock or
;; indentation might not yet work as expected.  Most convenience functions of
;; the old puppet-mode are not (yet) implemented.

;;; Code:

(require 'treesit)

(defcustom puppet-indent-level 2
  "Number of spaces for each indententation step."
  :group 'puppet
  :type 'integer
  :safe 'integerp)

(defcustom puppet-indent-tabs-mode nil
  "Indentation can insert tabs in puppet mode if this is non-nil."
  :type 'boolean
  :group 'puppet
  :safe 'booleanp)

(defvar puppet--binary-operators
  '("and" "or" "in")
  "Binary operators used by Puppet.")

(defvar puppet--boolean-constants
  '("true" "false")
  "Boolean constants used by Puppet.")

(defvar puppet--language-constants
  '("undef")
  "Language constants used by Puppet.")

(defvar puppet--file-attribute-constants
  '("file" "directory" "link")
  "Constants used for Puppet file resources.")

(defvar puppet--package-attribute-constants
  '("present" "absent" "installed" "latest")
  "Constants used for Puppet package resources.")

(defvar puppet--service-attribute-constants
  '("running" "stopped")
  "Constants used for Puppet service resources.")

;;
;; Regular expressions
;;

(defvar puppet--boolean-constants-regex
  (eval `(rx bos (or ,@puppet--boolean-constants) eos))
  "Regex to match Puppet boolean constants.")

(defvar puppet--attribute-name-constants-regex
  (eval `(rx bos (or ,@puppet--boolean-constants "default" "undef") eos))
  "Regex to match Puppet attribute name constants.")

(defvar puppet--constants-regex
  (eval `(rx bos
             (or ,@(append puppet--boolean-constants
                           puppet--file-attribute-constants
                           puppet--package-attribute-constants
                           puppet--service-attribute-constants))
             eos))
  "Puppet constants for tree-sitter font-locking.")

;; http://docs.puppetlabs.com/references/stable/metaparameter.html
(defvar puppet--metaparams-regex
  (rx bos
      (or "alias" "audit" "before" "consume" "export" "loglevel" "noop"
          "notify" "require" "schedule" "stage" "subscribe" "tag" "ensure")
      eos)
  "Puppet metaparameters for tree-sitter font-locking.
Strictly speakting, 'ensure' is not a real metaparameter, but it
is added here because it is common and important.")

;; https://puppet.com/docs/puppet/latest/function.html
(defvar puppet--builtin-functions-regex
  (rx bos
      (or "abs" "alert" "all" "annotate" "any" "assert_type" "binary_file"
          "break" "call" "camelcase" "capitalize" "ceiling" "chomp" "chop"
          "compare" "contain" "convert_to" "create_resources" "crit" "debug"
          "defined" "dig" "digest" "downcase" "each" "emerg" "empty" "epp"
          "err" "eyaml_lookup_key" "fail" "file" "filter" "find_file"
          "find_template" "flatten" "floor" "fqdn_rand" "generate" "get"
          "getvar" "group_by" "hiera" "hiera_array" "hiera_hash"
          "hiera_include" "hocon_data" "import" "include" "index" "info"
          "inline_epp" "inline_template" "join" "json_data" "keys" "length"
          "lest" "lookup" "lstrip" "map" "match" "max" "md5" "min"
          "module_directory" "new" "next" "notice" "partition" "realize"
          "reduce" "regsubst" "require" "return" "reverse_each" "round"
          "rstrip" "scanf" "sha1" "sha256" "shellquote" "size" "slice"
          "sort" "split" "sprintf" "step" "strftime" "strip" "tag" "tagged"
          "template" "then" "tree_each" "type" "unique" "unwrap" "upcase"
          "values" "versioncmp" "warning" "with" "yaml_data"
          ;; Bolt: https://puppet.com/docs/bolt/0.x/plan_functions.html
          "apply" "apply_prep" "add_facts" "facts" "fail_plan" "file_upload"
          "get_targets" "puppetdb_fact" "puppetdb_query" "run_command"
          "run_plan" "run_script" "run_task" "set_feature" "set_var" "vars"
          "without_default_logging")
      eos)
  "Internal functions provided by Puppet.")

;;
;; Faces
;;

(defface puppet-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments in Puppet."
  :group 'puppet)

(defface puppet-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings in Puppet."
  :group 'puppet)

(defface puppet-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords in Puppet."
  :group 'puppet)

(defface puppet-resource-type-face
  '((t :inherit font-lock-type-face))
  "Face for resource types in Puppet."
  :group 'puppet)

(defface puppet-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for built-in functions in Puppet."
  :group 'puppet)

(defface puppet-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for a constant in Puppet."
  :group 'puppet)

(defface puppet-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the name of a variable in Puppet."
  :group 'puppet)

(defface puppet-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for the name of a function in Puppet."
  :group 'puppet)

(defface puppet-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for language errors in Puppet found by the `tree-sitter' parser."
  :group 'puppet)

(defvar puppet-ts-mode--feature-list
  '((comment)
    (keyword resource-type builtin string)
    (constant variable string-interpolation)
    (error))
  "`treesit-font-lock-feature-list' for `puppet-ts-mode'.")

(defvar puppet-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :feature 'comment
   :language 'puppet
   '((comment) @puppet-comment-face)

   :feature 'string
   :language 'puppet
   '((string) @puppet-string-face)

   :feature 'string-interpolation
   :language 'puppet
   :override t
   '((string (interpolation) @puppet-variable-name-face))

   :feature 'variable
   :language 'puppet
   '((variable ["$" (identifier)] @puppet-variable-name-face)
     (variable (class_identifier ["$" (identifier)] @puppet-variable-name-face)))

   :feature 'constant
   :language 'puppet
   `((boolean [,@puppet--boolean-constants] @puppet-constant-face)
     (parameter (undef) @puppet-constant-face)
     (attribute value: (undef) @puppet-constant-face)
     (attribute name: (identifier) @puppet-constant-face
                (:match ,puppet--attribute-name-constants-regex
                        @puppet-constant-face))
     (attribute value: (identifier) @puppet-constant-face
                (:match ,puppet--constants-regex @puppet-constant-face)))

   :feature 'keyword
   :language 'puppet
   `((case_statement "case" @puppet-keyword-face)
     (default_case "default" @puppet-keyword-face)
     (function_declaration "function" @puppet-keyword-face)
     (if_statement "if" @puppet-keyword-face)
     (elsif_statement "elsif" @puppet-keyword-face)
     (else_statement "else" @puppet-keyword-face)
     (unless_statement "unless" @puppet-keyword-face)
     (node_definition "node" @puppet-keyword-face)
     (class_definition "class" @puppet-keyword-face)
     (class_inherits "inherits" @puppet-keyword-face)
     (defined_resource_type "define" @puppet-keyword-face)
     (attribute name: (identifier) @puppet-keyword-face
                (:match ,puppet--metaparams-regex @puppet-keyword-face))
     (binary_expression operator: [,@puppet--binary-operators]
                        @puppet-keyword-face)
     (field_expression (identifier) @puppet-keyword-face
                       (:match ,puppet--builtin-functions-regex
                               @puppet-keyword-face)))

   :feature 'resource-type
   :language 'puppet
   `((resource_declaration virtual: "@" @puppet-resource-type-face)
     (resource_declaration exported: "@@" @puppet-resource-type-face)
     (resource_declaration type: (identifier) @puppet-resource-type-face)
     (resource_reference (identifier) @puppet-resource-type-face)
     (class_definition (identifier) @puppet-resource-type-face)
     (class_identifier (identifier) @puppet-resource-type-face)
     (composite_type (identifier) @puppet-resource-type-face)
     (builtin_type _ @puppet-resource-type-face))

   :feature 'builtin
   :language 'puppet
   `((function_call (identifier) @font-lock-builtin-face
                    (:match ,puppet--builtin-functions-regex
                            @font-lock-builtin-face)))

   :feature 'error
   :language 'puppet
   '((ERROR) @puppet-warning-face))
  "Font-Lock settings for `puppet-ts-mode'.")

;;
;; Indentation
;;

(defvar puppet-indent-one-level
  (rx bos
      (or "block" "hash" "selector" "resource_declaration" "case_statement"
          "parameter_list" "array")
      eos)
  "Structures that will have their children indented by one more level.")

(defvar puppet-indent-like-parent
  (rx bos
      (or "else_statement" "elsif_statement")
      eos)
  "Statements that will be indented the same level as their parent.")

(defvar puppet-ts-indent-rules
  `((puppet
     ((parent-is "source_file")
      column-0 0)
     ;; block structures
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((parent-is ,puppet-indent-one-level) parent-bol puppet-indent-level)
     ;; compound statements
     ((node-is ,puppet-indent-like-parent) parent-bol 0)
     ;; default
     (no-node parent 0)))
  "Indentation rules for `puppet-ts-mode'.")

(defvar puppet-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Our strings
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    ;; C-style comments.
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?* ". 23b" table)
    ;; Line comments
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; The backslash is our escape character
    (modify-syntax-entry ?\\ "\\" table)
    ;; The dollar sign is an expression prefix for variables
    (modify-syntax-entry ?$ "'" table)
    ;; Fix various operators and punctionation.
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?\; "." table)
    ;; Our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table in use in `puppet-ts-mode' buffers.")

;;;###autoload
(define-derived-mode puppet-ts-mode prog-mode "Puppet[ts]"
  "Major mode for editing Puppet files, using tree-sitter library.

\\{puppet-ts-mode-map}"
  :syntax-table puppet-ts-mode-syntax-table

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local parse-sexp-ignore-comments t)

  ;; Indentation
  ;;(setq-local indent-line-function #'puppet-indent-line)
  (setq indent-tabs-mode puppet-indent-tabs-mode)
  (setq-local electric-indent-chars (append '(?\{ ?\}) electric-indent-chars))

  ;; Paragaphs
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-start "\f\\|[ \t]*$\\|#$")
  (setq-local paragraph-separate "\\([ \t\f]*\\|#\\)$")

  ;; Treesitter
  (when (treesit-ready-p 'puppet)
    (treesit-parser-create 'puppet)

    ;; Font-Lock
    (setq-local treesit-font-lock-feature-list puppet-ts-mode--feature-list)
    (setq-local treesit-font-lock-settings puppet-ts-mode--font-lock-settings)

    ;; Indentation
    (setq-local treesit-simple-indent-rules puppet-ts-indent-rules)
    (setq-local treesit--indent-verbose t)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-ts-mode))

(provide 'puppet-ts-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; puppet-ts-mode.el ends here
