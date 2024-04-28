;;; puppet-ts-mode.el --- Major mode for Puppet using Tree-sitter -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author: Stefan Möding
;; Version: 0.1.0
;; Created: <2024-03-02 13:05:03 stm>
;; Updated: <2024-04-28 13:43:12 stm>
;; URL: https://github.com/smoeding/puppet-ts-mode
;; Keywords: puppet, treesitter
;; Package-Requires: ((emacs "29.1"))

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

;; This package defines a tree-sitter enabled major mode for Puppet that
;; provides support for indentation, font-locking, imenu (not yet), and
;; structural navigation (not yet).
;;
;; Before the mode can be used, the tree-sitter parser for Puppet must be
;; installed.  This can be done by using the following Elisp snippet:
;;
;;    (add-to-list
;;     'treesit-language-source-alist
;;     '(puppet "https://github.com/smoeding/tree-sitter-puppet"))
;;    (treesit-install-language-grammar 'puppet)
;;
;; Note that a compiler toolchain is required for this to work.  You shoul
;; also consult the Emacs manual: (info "(elisp) Parsing Program Source")
;;
;; Caution: Currently this is work in progress; many details concerning
;; font-lock or indentation might not yet work as expected.  Most convenience
;; functions of the old puppet-mode are not (yet) implemented.

;;; Code:


;;; Requirements

(require 'treesit)

(eval-when-compile
  (require 'rx))


;;; Customization

(defgroup puppet nil
  "Write Puppet manifests in Emacs."
  :prefix "puppet-"
  :group 'languages)

(defvar puppet--file-attribute-constants
  '("file" "directory" "link")
  "Attributes for file resources formatted as constants.")

(defvar puppet--package-attribute-constants
  '("present" "absent" "installed" "latest")
  "Attributes for package resources formatted as constants.")

(defvar puppet--service-attribute-constants
  '("running" "stopped")
  "Attributes for service resources formatted as constants.")

;; https://www.puppet.com/docs/puppet/latest/metaparameter.html
(defvar puppet--metaparameters
  '("alias" "audit" "before" "consume" "export" "loglevel" "noop"
    "notify" "require" "schedule" "stage" "subscribe" "tag" "ensure")
  "Metaparameter attributes for all resource types.
Strictly speakting, \"ensure\" is not a real metaparameter, but it
is added here because it is common and important.")

;; https://www.puppet.com/docs/puppet/latest/function.html
(defvar puppet--builtin-functions
  '("abs" "alert" "all" "annotate" "any" "assert_type" "binary_file" "break"
    "call" "camelcase" "capitalize" "ceiling" "chomp" "chop" "compare"
    "contain" "convert_to" "create_resources" "crit" "debug" "defined" "dig"
    "digest" "downcase" "each" "emerg" "empty" "epp" "err" "eyaml_lookup_key"
    "fail" "file" "filter" "find_file" "find_template" "flatten" "floor"
    "fqdn_rand" "generate" "get" "getvar" "group_by" "hiera" "hiera_array"
    "hiera_hash" "hiera_include" "hocon_data" "import" "include" "index"
    "info" "inline_epp" "inline_template" "join" "json_data" "keys" "length"
    "lest" "lookup" "lstrip" "map" "match" "max" "md5" "min"
    "module_directory" "new" "next" "notice" "partition" "realize" "reduce"
    "regsubst" "require" "return" "reverse_each" "round" "rstrip" "scanf"
    "sha1" "sha256" "shellquote" "size" "slice" "sort" "split" "sprintf"
    "step" "strftime" "strip" "tag" "tagged" "template" "then" "tree_each"
    "type" "unique" "unwrap" "upcase" "values" "versioncmp" "warning" "with"
    "yaml_data"
    ;; Bolt: https://puppet.com/docs/bolt/0.x/plan_functions.html
    "apply" "apply_prep" "add_facts" "facts" "fail_plan" "file_upload"
    "get_targets" "puppetdb_fact" "puppetdb_query" "run_command" "run_plan"
    "run_script" "run_task" "set_feature" "set_var" "vars"
    "without_default_logging")
  "Internal functions provided by Puppet.")

(defvar puppet--statement-functions
  '("include" "require" "contain" "tag"       ; Catalog statements
    "debug" "info" "notice" "warning" "err"   ; Logging statements
    "fail")                                   ; Failure statements
  "Statement functions provided by Puppet.")

;; Regular expressions

(defvar puppet--constants-regex
  (rx-to-string `(seq bos
                      ,(cons 'or (append puppet--file-attribute-constants
                                         puppet--package-attribute-constants
                                         puppet--service-attribute-constants))
                      eos)
                'no-group)
  "Puppet constants for tree-sitter font-locking.")

(defvar puppet--metaparameters-regex
  (rx-to-string `(seq bos
                      ,(cons 'or puppet--metaparameters)
                      eos)
                'no-group)
  "Regex to match Puppet metaparameters.")

(defvar puppet--builtin-functions-regex
  (rx-to-string `(seq bos
                      ,(cons 'or puppet--builtin-functions)
                      eos)
                'no-group)
  "Internal functions provided by Puppet.")

(defvar puppet--statement-functions-regex
  (rx-to-string `(seq bos
                      ,(cons 'or puppet--statement-functions)
                      eos)
                'no-group)
  "Statement functions provided by Puppet.")


;;; Faces

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

(defface puppet-variable-use-face
  '((t :inherit font-lock-variable-use-face))
  "Face for the name of a variable being referenced in Puppet."
  :group 'puppet)

(defface puppet-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for the name of a function in Puppet."
  :group 'puppet)

(defface puppet-function-call-face
  '((t :inherit font-lock-function-call-face))
  "Face for the name of a function being called in Puppet."
  :group 'puppet)

(defface puppet-negation-char-face
  '((t :inherit font-lock-negation-char-face))
  "Face for negation characters."
  :group 'puppet)

(defface puppet-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for language errors found by the parser."
  :group 'puppet)


;; Font-Lock

(defvar puppet-ts-mode--feature-list
  ;; Level 1 usually contains only comments and definitions.
  ;; Level 2 usually adds keywords, strings, data types, etc.
  ;; Level 3 usually represents full-blown fontifications, including
  ;; assignments, constants, numbers and literals, etc.
  ;; Level 4 adds everything else that can be fontified: delimiters,
  ;; operators, brackets, punctuation, all functions, properties,
  ;; variables, etc.
  '((comment definition)
    (keyword function resource-type builtin string)
    (constant variable interpolation)
    (operator error))
  "`treesit-font-lock-feature-list' for `puppet-ts-mode'.")

(defvar puppet-ts-mode--font-lock-settings
  `(:feature comment
    :language puppet
    ((comment) @puppet-comment-face)

    :feature string
    :language puppet
    ((string) @puppet-string-face)

    :feature interpolation
    :language puppet
    :override t
    ((interpolation) @puppet-variable-name-face)

    :feature operator
    :language puppet
    ((unary operator: "!" @puppet-negation-char-face))

    :feature variable
    :language puppet
    ((variable ["$" (name)] @puppet-variable-name-face))

    :feature constant
    :language puppet
    (((true) @puppet-constant-face)
     ((false) @puppet-constant-face)
     ((default) @puppet-constant-face)
     ((undef) @puppet-constant-face))

    :feature definition
    :language puppet
    ((class_definition ["class" "inherits"] @puppet-keyword-face)
     (define_definition "define" @puppet-keyword-face)
     (function_definition "function" @puppet-keyword-face)
     (node_definition "node" @puppet-keyword-face)
     (plan_definition "plan" @puppet-keyword-face)
     ;; names of defined classes, defined types, functions, nodes, ...
     (classname (name) @puppet-resource-type-face)
     ;; hostnames in a node definition
     (hostname (dotted_name) @puppet-resource-type-face))

    :feature builtin
    :language puppet
    ((statement_function (name) @puppet-builtin-face
                         (:match ,puppet--statement-functions-regex
                                 @puppet-builtin-face))
     (function_call (name) @puppet-builtin-face
                    (:match ,puppet--builtin-functions-regex
                            @puppet-builtin-face))
     (named_access (name) @puppet-builtin-face
                   (:match ,puppet--builtin-functions-regex
                           @puppet-builtin-face))
     (attribute name: (name) @puppet-builtin-face
                (:match ,puppet--metaparameters-regex
                        @puppet-builtin-face))
     (attribute value: (name) @puppet-builtin-face
                (:match ,puppet--constants-regex
                        @puppet-builtin-face)))

    :feature function
    :language puppet
    ((function_call (name) @puppet-function-name-face))

    :feature keyword
    :language puppet
    (((if "if" @puppet-keyword-face))
     ((elsif "elsif" @puppet-keyword-face))
     ((else "else" @puppet-keyword-face))
     ((unless "unless" @puppet-keyword-face))
     ((case "unless" @puppet-keyword-face))
     (binary operator: ["and" "or" "in"] @puppet-keyword-face))

    :feature resource-type
    :language puppet
    (((resource_type [(name) (virtual) (exported)] @puppet-resource-type-face))
     ((statement_function
       (argument_list (argument (name) @puppet-resource-type-face))))
     ;; data and resource reference types
     ((type) @puppet-resource-type-face))

    :feature error
    :language puppet
    :override t
    ((ERROR) @puppet-warning-face))
  "`treesit-font-lock-settings' for `puppet-ts-mode'.")


;; Indentation

(defcustom puppet-indent-level 2
  "Number of spaces for each indententation step."
  :group 'puppet
  :type 'integer
  :safe 'integerp)

(defcustom puppet-indent-tabs-mode nil
  "Indentation can insert tabs in puppet mode if this is non-nil."
  :group 'puppet
  :type 'boolean
  :safe 'booleanp)

(defsubst puppet-find-ancestor-node (node regex)
  "Find ancestor of NODE with a node type that matched REGEX."
  (treesit-parent-until node
                        #'(lambda (x)
                            (string-match-p regex (treesit-node-type x)))
                        t))

(defun puppet-ancestor-definition-bol (node parent _bol)
  "Search ancestors of NODE or PARENT for a Puppet definition.

The search starts with PARENT if NODE is NIL.  This happens if no
node can start at the position, e.g. there is an empty line.
Return the beginning of line position for the Puppet definition."
  (let ((ancestor (puppet-find-ancestor-node
                   (or node parent)     ; Start with parent if node is nil
                   (regexp-opt '("class_definition" "define_definition"
                                 "function_definition")))))
    (if ancestor
        (save-excursion
          (goto-char (treesit-node-start ancestor))
          (back-to-indentation)
          (point)))))

(defun puppet-ancestor-resource-bol (node parent _bol)
  "Search ancestors of NODE or PARENT for a Puppet resource.

The search starts with PARENT if NODE is NIL.  This happens if no
node can start at the position, e.g. there is an empty line.
Return the beginning of line position for the Puppet resource."
  (let ((ancestor (puppet-find-ancestor-node
                   (or node parent)     ; Start with parent if node is nil
                   (regexp-opt '("resource_type" "resource_reference")))))
    (if ancestor
        (save-excursion
          (goto-char (treesit-node-start ancestor))
          (back-to-indentation)
          (point)))))

(setq treesit-simple-indent-presets
      (append treesit-simple-indent-presets
              (list (cons 'definition-bol #'puppet-ancestor-definition-bol)
                    (cons 'resource-bol #'puppet-ancestor-resource-bol))))

(defvar puppet-ts-indent-rules
  `((puppet
     ;; top-level statements start in column zero
     ((parent-is "manifest") column-0 0)
     ;; block structure
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol puppet-indent-level)
     ((node-is "elsif") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((node-is "case_option") parent-bol puppet-indent-level)
     ((node-is "selector_option") parent-bol puppet-indent-level)
     ((parent-is "resource_type") resource-bol puppet-indent-level)
     ((parent-is "resource_body") resource-bol puppet-indent-level)
     ((parent-is "resource_reference") resource-bol puppet-indent-level)
     ((node-is "attribute") resource-bol puppet-indent-level)
     ((parent-is "attribute_list") resource-bol puppet-indent-level)
     ((parent-is "parameter_list") definition-bol puppet-indent-level)
     (catch-all parent-bol 0)))
  "Indentation rules for `puppet-ts-mode'.")


;; Major mode definition

(defvar puppet-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; our strings
    (modify-syntax-entry ?\' "\"'"  table)
    (modify-syntax-entry ?\" "\"\"" table)
    ;; C-style comments
    (modify-syntax-entry ?/ ". 14b" table)
    (modify-syntax-entry ?* ". 23b" table)
    ;; line comments
    (modify-syntax-entry ?#  "<" table)
    (modify-syntax-entry ?\n ">" table)
    ;; the backslash is our escape character
    (modify-syntax-entry ?\\ "\\" table)
    ;; the dollar sign is an expression prefix for variables
    (modify-syntax-entry ?$ "'" table)
    ;; various operators and punctionation.
    (modify-syntax-entry ?<  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?&  "." table)
    (modify-syntax-entry ?|  "." table)
    (modify-syntax-entry ?%  "." table)
    (modify-syntax-entry ?=  "." table)
    (modify-syntax-entry ?+  "." table)
    (modify-syntax-entry ?-  "." table)
    (modify-syntax-entry ?\; "." table)
    ;; our parenthesis, braces and brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table)
  "Syntax table used in `puppet-ts-mode' buffers.")

(defvar puppet-ts-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Editing
    ;;(define-key map (kbd "C-c C-a") #'puppet-align-block)
    map)
  "Keymap for Puppet Mode buffers.")

;;;###autoload
(define-derived-mode puppet-ts-mode prog-mode "Puppet[ts]"
  "Major mode for editing Puppet files, using the tree-sitter library.

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
  (setq-local electric-indent-chars
              (append '(?\{ ?\} ?\( ?\) ?: ?,) electric-indent-chars))

  ;; Paragaphs
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-start "\f\\|[ \t]*$\\|#$")
  (setq-local paragraph-separate "\\([ \t\f]*\\|#\\)$")

  ;; Treesitter
  (when (treesit-ready-p 'puppet)
    (treesit-parser-create 'puppet)

    ;; Font-Lock
    (setq-local treesit-font-lock-feature-list puppet-ts-mode--feature-list)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       puppet-ts-mode--font-lock-settings))

    ;; Indentation
    (setq-local treesit-simple-indent-rules puppet-ts-indent-rules)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-ts-mode))

(provide 'puppet-ts-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; puppet-ts-mode.el ends here
