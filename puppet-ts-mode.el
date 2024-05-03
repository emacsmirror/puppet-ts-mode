;;; puppet-ts-mode.el --- Major mode for Puppet using Tree-sitter -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author:           Stefan Möding <stm@kill-9.net>
;; Maintainer:       Stefan Möding <stm@kill-9.net>
;; Version:          0.1.0
;; Created:          <2024-03-02 13:05:03 stm>
;; Updated:          <2024-05-03 10:16:44 stm>
;; URL:              https://github.com/smoeding/puppet-ts-mode
;; Keywords:         languages, puppet, tree-sitter
;; Package-Requires: ((emacs "29.1") (cycle-quotes "0.1"))

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

;; This package provides Puppet syntax highlighting, indentation and
;; navigation using Tree-sitter.  To use the `puppet-ts-mode' major mode you
;; will need to install the appropriate grammar.  This can be done by using
;; the following Elisp snippet:
;;
;;    (add-to-list
;;     'treesit-language-source-alist
;;     '(puppet "https://github.com/smoeding/tree-sitter-puppet"))
;;    (treesit-install-language-grammar 'puppet)
;;
;; Note that a compiler toolchain is required for this to work.  You should
;; also consult the Emacs manual: (info "(elisp) Parsing Program Source")
;;
;; Caution: Currently this is work in progress; many details concerning
;; font-lock or indentation might not yet work as expected.  Most convenience
;; functions of the old puppet-mode are not (yet) implemented.

;;; Code:


;;; Requirements

(require 'treesit)
(require 'align)
(require 'xref)

(eval-when-compile
  (require 'cl-lib)
  (require 'skeleton)
  (require 'rx))


;;; Customization

(defgroup puppet-ts nil
  "Write Puppet manifests in Emacs."
  :prefix "puppet-ts-"
  :group 'languages)

(defvar puppet-ts--file-attribute-constants
  '("file" "directory" "link")
  "Attributes for file resources formatted as constants.")

(defvar puppet-ts--package-attribute-constants
  '("present" "absent" "installed" "latest")
  "Attributes for package resources formatted as constants.")

(defvar puppet-ts--service-attribute-constants
  '("running" "stopped")
  "Attributes for service resources formatted as constants.")

;; https://www.puppet.com/docs/puppet/latest/metaparameter.html
(defvar puppet-ts--metaparameters
  '("alias" "audit" "before" "consume" "export" "loglevel" "noop"
    "notify" "require" "schedule" "stage" "subscribe" "tag" "ensure")
  "Metaparameter attributes for all resource types.
Strictly speakting, \"ensure\" is not a real metaparameter, but it
is added here because it is common and important.")

;; https://www.puppet.com/docs/puppet/latest/function.html
(defvar puppet-ts--builtin-functions
  '("abs" "alert" "all" "annotate" "any" "assert_type" "binary_file"
    "break" "call" "camelcase" "capitalize" "ceiling" "chomp" "chop"
    "compare" "convert_to" "create_resources" "defined" "dig" "digest"
    "downcase" "each" "emerg" "empty" "epp" "eyaml_lookup_key" "file"
    "filter" "find_file" "find_template" "flatten" "floor" "fqdn_rand"
    "generate" "get" "getvar" "group_by" "hiera" "hiera_array"
    "hiera_hash" "hiera_include" "hocon_data" "import" "index"
    "inline_epp" "inline_template" "join" "json_data" "keys" "length"
    "lest" "lookup" "lstrip" "map" "match" "max" "md5" "min"
    "module_directory" "new" "next" "partition" "realize" "reduce"
    "regsubst" "return" "reverse_each" "round" "rstrip" "scanf" "sha1"
    "sha256" "shellquote" "size" "slice" "sort" "split" "sprintf"
    "step" "strftime" "strip" "tagged" "template" "then" "tree_each"
    "type" "unique" "unwrap" "upcase" "values" "versioncmp" "with"
    "yaml_data"
    ;; Bolt: https://puppet.com/docs/bolt/0.x/plan_functions.html
    "apply" "apply_prep" "add_facts" "facts" "fail_plan" "file_upload"
    "get_targets" "puppetdb_fact" "puppetdb_query" "run_command"
    "run_plan" "run_script" "run_task" "set_feature" "set_var" "vars"
    "without_default_logging")
  "Internal functions provided by Puppet.")

(defvar puppet-ts--statement-functions
  '("include" "require" "contain" "tag"            ; Catalog statements
    "debug" "info" "notice" "warning" "err" "crit" ; Logging statements
    "fail")                                        ; Failure statements
  "Statement functions provided by Puppet.")

;; Regular expressions

(defvar puppet-ts--constants-regex
  (rx-to-string `(seq bos
                      ,(cons 'or (append puppet-ts--service-attribute-constants
                                         puppet-ts--package-attribute-constants
                                         puppet-ts--file-attribute-constants))
                      eos)
                'no-group)
  "Puppet constants for tree-sitter font-locking.")

(defvar puppet-ts--metaparameters-regex
  (rx-to-string `(seq bos
                      ,(cons 'or puppet-ts--metaparameters)
                      eos)
                'no-group)
  "Regex to match Puppet metaparameters.")

(defvar puppet-ts--builtin-functions-regex
  (rx-to-string `(seq bos
                      ,(cons 'or (append puppet-ts--builtin-functions
                                         puppet-ts--statement-functions))
                      eos)
                'no-group)
  "Internal functions provided by Puppet.")

(defvar puppet-ts--statement-functions-regex
  (rx-to-string `(seq bos
                      ,(cons 'or puppet-ts--statement-functions)
                      eos)
                'no-group)
  "Statement functions provided by Puppet.")


;;; Faces

(defface puppet-ts-comment
  '((t :inherit font-lock-comment-face))
  "Face for comments in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-string
  '((t :inherit font-lock-string-face))
  "Face for strings in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-keyword
  '((t :inherit font-lock-keyword-face))
  "Face for keywords in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-resource-type
  '((t :inherit font-lock-type-face))
  "Face for resource types in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-builtin
  '((t :inherit font-lock-builtin-face))
  "Face for built-in functions in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-constant
  '((t :inherit font-lock-constant-face))
  "Face for a constant in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-variable-name
  '((t :inherit font-lock-variable-name-face))
  "Face for the name of a variable in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-variable-use
  '((t :inherit font-lock-variable-use-face))
  "Face for the name of a variable being referenced in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-function-name
  '((t :inherit font-lock-function-name-face))
  "Face for the name of a function in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-function-call
  '((t :inherit font-lock-function-call-face))
  "Face for the name of a function being called in Puppet."
  :group 'puppet-ts)

(defface puppet-ts-operator
  '((t :inherit font-lock-operator-face))
  "Face for operators."
  :group 'puppet-ts)

(defface puppet-ts-negation-char
  '((t :inherit font-lock-negation-char-face))
  "Face for negation characters."
  :group 'puppet-ts)

(defface puppet-ts-number
  '((t :inherit font-lock-number-face))
  "Face for numbers."
  :group 'puppet-ts)

(defface puppet-ts-warning
  '((t :inherit font-lock-warning-face))
  "Face for language errors found by the parser."
  :group 'puppet-ts)


;; Font-Lock

(defvar puppet-ts-mode-feature-list
  ;; Level 1 usually contains only comments and definitions.
  ;; Level 2 usually adds keywords, strings, data types, etc.
  ;; Level 3 usually represents full-blown fontifications, including
  ;; assignments, constants, numbers and literals, etc.
  ;; Level 4 adds everything else that can be fontified: delimiters,
  ;; operators, brackets, punctuation, all functions, properties,
  ;; variables, etc.
  '((comment definition)
    (keyword resource-type string)
    (constant number variable interpolation builtin function)
    (operator error))
  "`treesit-font-lock-feature-list' for `puppet-ts-mode'.")

(defvar puppet-ts-mode-font-lock-settings
  `( ;;
    :feature comment
    :language puppet
    ((comment) @puppet-ts-comment)

    :feature string
    :language puppet
    ((string) @puppet-ts-string)

    :feature interpolation
    :language puppet
    :override t
    ((interpolation) @puppet-ts-variable-name)

    :feature variable
    :language puppet
    ((variable ["$" (name)] @puppet-ts-variable-name))

    :feature constant
    :language puppet
    (((true) @puppet-ts-constant)
     ((false) @puppet-ts-constant)
     ((default) @puppet-ts-constant)
     ((undef) @puppet-ts-constant))

    :feature number
    :language puppet
    ((number) @puppet-ts-number)

    :feature definition
    :language puppet
    ((class_definition ["class" "inherits"] @puppet-ts-keyword)
     (define_definition "define" @puppet-ts-keyword)
     (function_definition "function" @puppet-ts-keyword)
     (node_definition "node" @puppet-ts-keyword)
     (plan_definition "plan" @puppet-ts-keyword)
     ;; names of defined classes, defined types, functions, nodes, ...
     (classname (name) @puppet-ts-resource-type)
     ;; hostnames in a node definition
     (hostname (dotted_name) @puppet-ts-resource-type))

    :feature builtin
    :language puppet
    ((statement_function (name) @puppet-ts-builtin
                         (:match ,puppet-ts--statement-functions-regex
                                 @puppet-ts-builtin))
     (function_call (name) @puppet-ts-builtin
                    (:match ,puppet-ts--builtin-functions-regex
                            @puppet-ts-builtin))
     (named_access (name) @puppet-ts-builtin
                   (:match ,puppet-ts--builtin-functions-regex
                           @puppet-ts-builtin))
     (attribute name: (name) @puppet-ts-builtin
                (:match ,puppet-ts--metaparameters-regex
                        @puppet-ts-builtin))
     (attribute value: (name) @puppet-ts-builtin
                (:match ,puppet-ts--constants-regex
                        @puppet-ts-builtin)))

    :feature function
    :language puppet
    ((function_call (name) @puppet-ts-function-name))

    :feature keyword
    :language puppet
    (((if "if" @puppet-ts-keyword))
     ((elsif "elsif" @puppet-ts-keyword))
     ((else "else" @puppet-ts-keyword))
     ((unless "unless" @puppet-ts-keyword))
     ((case "case" @puppet-ts-keyword))
     (binary operator: ["and" "or" "in"] @puppet-ts-keyword))

    :feature resource-type
    :language puppet
    (((resource_type [(name) (virtual) (exported)] @puppet-ts-resource-type))
     ;; arguments to statement functions
     ((statement_function
       (argument_list (argument (name) @puppet-ts-resource-type))))
     ;; data and resource reference types
     ((type) @puppet-ts-resource-type))

    :feature operator
    :language puppet
    ((unary operator: "!" @puppet-ts-negation-char)
     (unary operator: _  @puppet-ts-operator)
     (binary operator: _ @puppet-ts-operator))

    :feature error
    :language puppet
    :override t
    ((ERROR) @puppet-ts-warning))
  "`treesit-font-lock-settings' for `puppet-ts-mode'.")


;; Indentation

(defcustom puppet-ts-indent-level 2
  "Number of spaces for each indententation step."
  :group 'puppet-ts
  :type 'integer
  :safe 'integerp)

(defcustom puppet-ts-indent-tabs-mode nil
  "Indentation can insert tabs in puppet mode if this is non-nil."
  :group 'puppet-ts
  :type 'boolean
  :safe 'booleanp)

(defsubst puppet-ts-find-ancestor-node (node regex)
  "Find ancestor of NODE with a node type that matched REGEX."
  (treesit-parent-until node
                        (lambda (x)
                          (string-match-p regex (treesit-node-type x)))
                        t))

(defun puppet-ts-ancestor-definition-bol (node parent _bol)
  "Search ancestors of NODE or PARENT for a Puppet definition.

The search starts with PARENT if NODE is NIL.  This happens if no
node can start at the position, e.g. there is an empty line.
Return the beginning of line position for the Puppet definition.

The signature of this function is defined by Tree-Sitter."
  (let ((ancestor (puppet-ts-find-ancestor-node
                   (or node parent)     ; Start with parent if node is nil
                   (regexp-opt '("class_definition" "define_definition"
                                 "function_definition")))))
    (if ancestor
        (save-excursion
          (goto-char (treesit-node-start ancestor))
          (back-to-indentation)
          (point)))))

(defun puppet-ts-ancestor-resource-bol (node parent _bol)
  "Search ancestors of NODE or PARENT for a Puppet resource.

The search starts with PARENT if NODE is NIL.  This happens if no
node can start at the position, e.g. there is an empty line.
Return the beginning of line position for the Puppet resource.

The signature of this function is defined by Tree-Sitter."
  (let ((ancestor (puppet-ts-find-ancestor-node
                   (or node parent)     ; Start with parent if node is nil
                   (regexp-opt '("resource_type" "resource_reference")))))
    (if ancestor
        (save-excursion
          (goto-char (treesit-node-start ancestor))
          (back-to-indentation)
          (point)))))

(defun puppet-ts-ancestor-function-bol (node parent _bol)
  "Search ancestors of NODE or PARENT for a Puppet function call.

The search starts with PARENT if NODE is NIL.  This happens if no
node can start at the position, e.g. there is an empty line.
Return the beginning of line position for the Puppet resource.

The signature of this function is defined by Tree-Sitter."
  (let ((ancestor (puppet-ts-find-ancestor-node
                   (or node parent)     ; Start with parent if node is nil
                   (regexp-opt '("function_call")))))
    (if ancestor
        (save-excursion
          (goto-char (treesit-node-start ancestor))
          (back-to-indentation)
          (point)))))

;; Make the custom function usable as indent anchors by tree-sitter
(setq treesit-simple-indent-presets
      (append treesit-simple-indent-presets
              (list (cons 'definition-bol #'puppet-ts-ancestor-definition-bol)
                    (cons 'resource-bol #'puppet-ts-ancestor-resource-bol)
                    (cons 'function-bol #'puppet-ts-ancestor-function-bol))))

(defvar puppet-ts-indent-rules
  `((puppet
     ;; top-level statements start in column zero
     ((parent-is "manifest") column-0 0)
     ;; blocks
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol puppet-ts-indent-level)
     ;; compound statements
     ((node-is "elsif") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ;; arrays
     ((match "array_element" nil nil 1 1) parent-bol puppet-ts-indent-level)
     ((match "array_element" nil nil 2 nil) prev-sibling 0)
     ((parent-is "array") parent-bol puppet-ts-indent-level)
     ;; structures and expressions
     ((parent-is "case") parent-bol puppet-ts-indent-level)
     ((parent-is "hash") parent-bol puppet-ts-indent-level)
     ((parent-is "selector") parent-bol puppet-ts-indent-level)
     ((parent-is "function_call") parent-bol puppet-ts-indent-level)
     ((parent-is "resource_type") resource-bol puppet-ts-indent-level)
     ((parent-is "resource_body") resource-bol puppet-ts-indent-level)
     ((parent-is "resource_reference") resource-bol puppet-ts-indent-level)
     ((node-is "attribute") resource-bol puppet-ts-indent-level)
     ((parent-is "attribute_list") resource-bol puppet-ts-indent-level)
     ((parent-is "parameter_list") definition-bol puppet-ts-indent-level)
     ((parent-is "argument_list") function-bol puppet-ts-indent-level)
     (catch-all parent-bol 0)))
  "Indentation rules for `puppet-ts-mode'.")


;; Alignment

(add-to-list 'align-sq-string-modes 'puppet-mode)
(add-to-list 'align-dq-string-modes 'puppet-mode)
(add-to-list 'align-open-comment-modes 'puppet-mode)

(defconst puppet-ts-mode-align-rules
  '((puppet-resource-arrow
     (regexp . "\\(\\s-*\\)=>\\(\\s-*\\)")
     (group  . (1 2))
     (modes  . '(puppet-ts-mode))
     (separate . entire))
    (puppet-param-default
     (regexp . "\\(\\s-+\\)$[a-z_][a-zA-Z0-9_]*\\(\\s-*\\)=\\(\\s-*\\)")
     (group  . (1 2 3))
     (modes  . '(puppet-ts-mode)))
    (puppet-param-nodefault
     (regexp . "\\(\\s-+\\)$[a-z_][a-zA-Z0-9_]*")
     (modes  . '(puppet-ts-mode))))
  "Align rules for Puppet attributes and parameters.")

(defconst puppet-ts-mode-align-exclude-rules
  '((puppet-nested
     (regexp . "\\s-*=>\\s-*\\({[^}]*}\\)")
     (modes  . '(puppet-ts-mode))
     (separate . entire))
    (puppet-comment
     (regexp . "^\\s-*#\\(.*\\)")
     (modes . '(puppet-ts-mode))))
  "Rules for excluding lines from alignment for Puppet.")

(defconst puppet-ts-align-node-types-regex
  (rx (or "hash" "parameter_list" "resource_type" "resource_reference"))
  "List of parser items that can be aligned.")

(defun puppet-ts-find-alignment-node (location)
  "Identify the innermost node of a block that can be aligned.

Walk the parse tree upwards starting from LOCATION and check the
nodes we find.  Terminate the search if we know how to align the
current node.  The constant `puppet-ts-align-node-types-regex'
has the regex of the acceptable node types.

Return the node or nil if nothing is found."
  (save-excursion
    (cl-loop for     node = (treesit-node-on location location)
             then    (treesit-node-parent node)
             always  node
             for     type = (treesit-node-type node)
             until   (string-match-p puppet-ts-align-node-types-regex type)
             ;;do    (message "check align node %s" type)
             finally return node)))

(defun puppet-ts-align-block ()
  "Align the current parameter or attribute block.

The current block is the innermost block that point is in."
  (interactive "*")
  (when-let* ((node (puppet-ts-find-alignment-node (point)))
              (beg (treesit-node-start node))
              (end (treesit-node-end node)))
    ;;(message "about to align %S" (treesit-node-type node))
    (pcase (treesit-node-type node)
      ("parameter_list" (align beg end))
      ("resource_type" (align beg end))
      ("resource_reference" (align beg end)))))


;;; Skeletons

(defun puppet-ts-dissect-filename (file)
  "Return list of path components for FILE.
The first list element is the basename of FILE and the remaining
elements are the names of each directory from FILE to the root of
the filesystem."
  (cl-loop for path = (file-name-sans-extension file)
           then (directory-file-name (file-name-directory path))
           ;; stop iteration at the root of the directory
           ;; tree (should work for Windows & Unix/Linux)
           until (or (string-suffix-p ":" path)
                     (string-equal (file-name-directory path) path))
           collect (file-name-nondirectory path)))

(defun puppet-ts-filename-parser (file)
  "Return list of path components for the Puppet manifest FILE.
The first element of the list will be the module name and the
remaining elements are the relative path components below the
‘manifests’ subdirectory.  The names of the path components are
only derived from the file name by using the Puppet auto-loader
rules.  FILE must be an absolute file name.

The module name \"unidentified\" is returned if a module name
can't be inferred from the file name.

If the directory name contains characters that are not legal for
a Puppet module name, then all leading characters including the
last illegal character are removed from the module name.  The
function will for example return ‘foo’ as module name even if the
module is using the ‘puppet-foo’ directory (e.g. for module
development in a user's home directory)."
  (if (stringp file)
      (let* ((parts (puppet-ts-dissect-filename file))
             ;; Remove "init" if it is the first element
             (compact (if (string-equal (car parts) "init")
                          (cdr parts)
                        parts)))
        (cons
         ;; module name with illegal prefixes removed or "unidentified" if
         ;; path is not compliant with the standard Puppet file hierarchy
         (replace-regexp-in-string
          "^.*[^a-z0-9_]" "" (or (cadr (member "manifests" parts))
                                 "unidentified"))
         ;; remaining path components
         (cdr (member "manifests" (reverse compact)))))
    '("unidentified")))

(defun puppet-ts-file-module-name (file)
  "Return the module name for the Puppet class in FILE."
  (car (puppet-ts-filename-parser file)))

(defun puppet-ts-file-class-name (file)
  "Return the class name for the Puppet class in FILE."
  (mapconcat #'identity (puppet-ts-filename-parser file) "::"))

(defun puppet-ts-file-type-name (file)
  "Return the Puppet type name for FILE.
FILE must be an absolute path name and should conform to the
standard Puppet module layout.  The Puppet type name is returned
if can be derived from FILE.  Otherwise NIL is returned.

If the function is called with the file name of a provider, the
appropriate type name is returned."
  (let ((path (puppet-ts-dissect-filename file)))
    (cond ((and (equal (nth 1 path) "type")
                (equal (nth 2 path) "puppet"))
           (car path))
          ((and (equal (nth 2 path) "provider")
                (equal (nth 3 path) "puppet"))
           (cadr path)))))

(defun puppet-ts-file-provider-name (file)
  "Return the Puppet provider name for FILE.
FILE must be an absolute path name and should conform to the
standard Puppet module layout.  The provider name is returned if
it can be derived from FILE.  Otherwise NIL is returned."
  (let ((path (puppet-ts-dissect-filename file)))
    (if (and (equal (nth 2 path) "provider")
             (equal (nth 3 path) "puppet"))
        (car path))))

(define-skeleton puppet-ts-keyword-class
  "Insert \"class\" skeleton."
  nil
  "class " (puppet-ts-file-class-name (buffer-file-name)) " (" > \n
  ") {" > \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-define
  "Insert \"class\" skeleton."
  nil
  "define " (puppet-ts-file-class-name (buffer-file-name)) " (" > \n
  ") {" > \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-node
  "Insert \"node\" skeleton."
  nil
  "node " > - " {" \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-if
  "Insert \"if\" statement."
  nil
  "if " > - " {" \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-elsif
  "Insert \"elsif\" statement."
  nil
  "elsif " > - " {" \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-else
  "Insert \"else\" statement."
  nil
  "else {" > \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-unless
  "Insert \"unless\" statement."
  nil
  "unless " > - " {" \n
  > _ "}" > \n)

(define-skeleton puppet-ts-keyword-case
  "Insert \"case\" statement."
  nil
  "case " > - " {" \n
  "default: {" > \n
  "}" > \n
  "}" > \n)

(define-skeleton puppet-ts-keyword-selector
  "Insert \"?\" selector."
  nil
  "? {" > \n
  "default => " > - "," \n
  "}" > \n)

(define-skeleton puppet-ts-type-anchor
  "Insert the \"anchor\" resource type."
  nil
  "anchor { " > - ": }" \n)

(define-skeleton puppet-ts-type-class
  "Insert the \"class\" resource type."
  nil
  "class { " > - ":" \n
  "}" > \n)

(define-skeleton puppet-ts-type-exec
  "Insert the \"exec\" resource type."
  nil
  "exec { " > - ":" \n
  "path => [ '/bin', '/sbin', '/usr/bin', '/usr/sbin', ]," > \n
  "user => 'root'," > \n
  "cwd  => '/'," > \n
  "}" > \n)

(define-skeleton puppet-ts-type-file
  "Insert the \"file\" resource type."
  nil
  "file { " > - ":" \n
  "ensure => file," > \n
  "owner  => 'root'," > \n
  "group  => 'root'," > \n
  "mode   => '0644'," > \n
  "}" > \n)

(define-skeleton puppet-ts-type-group
  "Insert the \"group\" resource type."
  nil
  "group { " > - ":" \n
  "ensure => present," > \n
  "}" > \n)

(define-skeleton puppet-ts-type-host
  "Insert the \"host\" resource type."
  nil
  "host { " > - ":" \n
  "ensure => present," > \n
  "}" > \n)

(define-skeleton puppet-ts-type-notify
  "Insert the \"notify\" resource type."
  nil
  "notify { " > - ": }" \n)

(define-skeleton puppet-ts-type-package
  "Insert the \"package\" resource type."
  nil
  "package { " > - ":" \n
  "ensure => present," > \n
  "}" > \n)

(define-skeleton puppet-ts-type-service
  "Insert the \"service\" resource type."
  nil
  "service { " > - ":" \n
  "ensure => running," > \n
  "enable => true," > \n
  "}" > \n)

(define-skeleton puppet-ts-type-user
  "Insert the \"user\" resource type."
  nil
  "user { " > - ":" \n
  "ensure   => present," > \n
  "shell    => '/bin/bash'," > \n
  "password => '*'," > \n
  "}" > \n)


;; Xref

(defcustom puppet-ts-module-path
  '("/etc/puppetlabs/code/environments/production/modules")
  "Directories to search for modules when resolving cross references.

The list can contain multiple directories to allow more than
a single search location (for example to have a local directory
tree for development).  Each directory should be a top-level
directory that has the module directories as subdirectories.  The
list is searched in order and the search is terminated when the
first match is found.

Remote directories as defined by TRAMP are possible but slow when
accessed."
  :group 'puppet-ts
  :type '(repeat directory))

(defun puppet-ts-module-root (file)
  "Return the Puppet module root directory for FILE.

Walk up the directory tree until a directory is found, that
either contains a \"manifests\", \"lib\" or \"types\" subdir.
Return the directory name or nil if no directory is found."
  (locate-dominating-file
   file
   (lambda (path)
     (and (file-accessible-directory-p path)
          (or (file-readable-p (expand-file-name "manifests" path))
              (file-readable-p (expand-file-name "lib" path))
              (file-readable-p (expand-file-name "types" path)))))))

(defun puppet-ts-autoload-path (identifier &optional directory extension)
  "Resolve IDENTIFIER into Puppet module and relative autoload path.

Use DIRECTORY as module subdirectory \(defaults to \"manifests\"
and EXTENSION as file extension \(defaults to \".pp\") when
building the path.

Return a cons cell where the first part is the module name and
the second part is a relative path name below that module where
the identifier should be defined according to the Puppet autoload
rules."
  (let* ((components (split-string identifier "::"))
         (module (car components))
         (path (cons (or directory "manifests")
                     (butlast (cdr components))))
         (file (if (cdr components)
                   (car (last components))
                 "init")))
    (cons module
          (concat (mapconcat #'file-name-as-directory path "")
                  file
                  (or extension ".pp")))))

(defun puppet-ts--xref-backend ()
  "The Xref backend for `puppet-ts-mode'."
  'puppet)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql puppet)))
  "Return the Puppet identifier at point."
  (let ((thing (thing-at-point 'symbol)))
    (and thing (substring-no-properties thing))))

(cl-defmethod xref-backend-definitions ((_backend (eql puppet)) identifier)
  "Find the definitions of a Puppet resource IDENTIFIER.

First the location of the visited file is checked.  Then all
directories from `puppet-ts-module-path' are searched for the module
and the file according to Puppet's autoloading rules."
  (let* ((resource (downcase (if (string-prefix-p "::" identifier)
                                 (substring identifier 2)
                               identifier)))
         (pupfiles (puppet-ts-autoload-path resource))
         (typfiles (puppet-ts-autoload-path resource "types"))
         (funfiles (puppet-ts-autoload-path resource "functions"))
         (xrefs '()))
    (if pupfiles
        (let* ((module (car pupfiles))
               ;; merged list of relative path names to classes/defines/types
               (pathlist (mapcar #'cdr (list pupfiles typfiles funfiles)))
               ;; list of directories where this module might be
               (moddirs (mapcar (lambda (dir) (expand-file-name module dir))
                                puppet-ts-module-path))
               ;; the regexp to find the resource definition in the file
               (resdef (concat "^\\(class\\|define\\|type\\|function\\)\\s-+"
                               resource
                               "\\((\\|{\\|\\s-\\|$\\)"))
               ;; files to visit when searching for the resource
               (files '()))
          ;; Check the current module directory (if the buffer actually
          ;; visits a file) and all module subdirectories from
          ;; `puppet-ts-module-path'.
          (dolist (dir (if buffer-file-name
                           (cons (puppet-ts-module-root buffer-file-name)
                                 moddirs)
                         moddirs))
            ;; Try all relative path names below the module directory that
            ;; might contain the resource; save the file name if the file
            ;; exists and we haven't seen it (we might try to check a file
            ;; twice if the current module is also below one of the dirs in
            ;; `puppet-ts-module-path').
            (dolist (path pathlist)
              (let ((file (expand-file-name path dir)))
                (if (and (not (member file files))
                         (file-readable-p file))
                    (setq files (cons file files))))))
          ;; Visit all found files to finally locate the resource definition
          (dolist (file files)
            (with-temp-buffer
              (insert-file-contents-literally file)
              (save-match-data
                (when (re-search-forward resdef nil t)
                  (push (xref-make
                         (match-string-no-properties 0)
                         (xref-make-file-location
                          file (line-number-at-pos (match-beginning 1)) 0))
                        xrefs)))))))
    xrefs))


;; Language grammar

(defconst puppet-ts-mode-treesit-language-source
  '(puppet . ("https://github.com/smoeding/tree-sitter-puppet" "main"))
  "The language source entry for `treesit-language-source-alist'.")

(defun puppet-ts-mode-install-grammar ()
  "Install the language grammar for `puppet-ts-mode'."
  (interactive)
  ;; Remove the entry if it exists
  (setq treesit-language-source-alist
        (assq-delete-all 'puppet treesit-language-source-alist))
  ;; Add correct entry
  (add-to-list 'treesit-language-source-alist
               puppet-ts-mode-treesit-language-source)
  ;; Install the grammar
  (treesit-install-language-grammar 'puppet))


;; User callable functions

(defun puppet-ts-interpolate (suppress)
  "Insert \"${}\" when point is in a double quoted string.

A single \"$\" is inserted if point is not in a double quoted
string.  With prefix argument SUPPRESS the braces are always left
out."
  (interactive "P*")
  (self-insert-command 1)
  (unless suppress
    (let ((node (treesit-parent-until
                 (treesit-node-at (point))
                 (lambda (x) (string= (treesit-node-type x) "string"))
                 t)))
      ;; Check if we are inside a string node and the string is terminated by
      ;; a " character.
      (if (and node (= (char-before (treesit-node-end node)) ?\"))
          (if mark-active
              (progn
                ;; Surround the region
                (goto-char (region-beginning))
                (insert "{")
                (goto-char (region-end))
                (insert "}"))
            (insert "{}")
            (forward-char -1))))))


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
    (define-key map (kbd "C-c C-a") #'puppet-ts-align-block)
    ;; (define-key map (kbd "C-c C-;") #'puppet-clear-string)
    (define-key map (kbd "$") #'puppet-ts-interpolate)
    ;; Skeletons for types
    (define-key map (kbd "C-c C-t a") #'puppet-ts-type-anchor)
    (define-key map (kbd "C-c C-t c") #'puppet-ts-type-class)
    (define-key map (kbd "C-c C-t e") #'puppet-ts-type-exec)
    (define-key map (kbd "C-c C-t f") #'puppet-ts-type-file)
    (define-key map (kbd "C-c C-t g") #'puppet-ts-type-group)
    (define-key map (kbd "C-c C-t h") #'puppet-ts-type-host)
    (define-key map (kbd "C-c C-t n") #'puppet-ts-type-notify)
    (define-key map (kbd "C-c C-t p") #'puppet-ts-type-package)
    (define-key map (kbd "C-c C-t s") #'puppet-ts-type-service)
    (define-key map (kbd "C-c C-t u") #'puppet-ts-type-user)
    ;; Skeletons for keywords
    (define-key map (kbd "C-c C-k c") #'puppet-ts-keyword-class)
    (define-key map (kbd "C-c C-k d") #'puppet-ts-keyword-define)
    (define-key map (kbd "C-c C-k n") #'puppet-ts-keyword-node)
    (define-key map (kbd "C-c C-k i") #'puppet-ts-keyword-if)
    (define-key map (kbd "C-c C-k e") #'puppet-ts-keyword-elsif)
    (define-key map (kbd "C-c C-k o") #'puppet-ts-keyword-else)
    (define-key map (kbd "C-c C-k u") #'puppet-ts-keyword-unless)
    (define-key map (kbd "C-c C-k s") #'puppet-ts-keyword-case)
    (define-key map (kbd "C-c C-k ?") #'puppet-ts-keyword-selector)
    map)
  "Keymap for Puppet Mode buffers.")

;;;###autoload
(define-derived-mode puppet-ts-mode prog-mode "Puppet[ts]"
  "Major mode for editing Puppet files, using the tree-sitter library.

Typing a \"$\" character inside a double quoted string will
trigger the variable interpolation syntax.  The \"$\" character
will be followed by a pair of braces so that the variable name to
be interpolated can be entered immediately.  The region will be
used as variable name if it is active when the \"$\" character is
entered.

The mode supports the cross-referencing system documented in the
Info node `Xref'.  The variable `puppet-ts-module-path' contains
a list of directories that are searched to find installed Puppet
modules.

Calling the function `xref-find-definitions' (bound to \\[xref-find-definitions])
with point on an identifier (a class, defined type, data type or
custom function) jumps to the definition of that identifier.
This is quick and does not need any sort of database, since the
name of the source file can be infered from the identifier.

The mode needs a tree-sitter grammar to be able to parse Puppet
code.  The grammar matching the package version can be installed
using the function `puppet-ts-mode-install-grammar'.

\\{puppet-ts-mode-map}"
  :syntax-table puppet-ts-mode-syntax-table

  ;; Comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[ \t]*")
  (setq-local parse-sexp-ignore-comments t)

  ;; Indentation
  ;;(setq-local indent-line-function #'puppet-ts-indent-line)
  (setq indent-tabs-mode puppet-ts-indent-tabs-mode)
  (setq-local electric-indent-chars
              (append '(?\{ ?\} ?\( ?\) ?: ?,) electric-indent-chars))

  ;; Paragaphs
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local paragraph-start "\f\\|[ \t]*$\\|#$")
  (setq-local paragraph-separate "\\([ \t\f]*\\|#\\)$")

  ;; Xref
  (add-hook 'xref-backend-functions #'puppet-ts--xref-backend)

  ;; Alignment
  (setq align-mode-rules-list puppet-ts-mode-align-rules)
  (setq align-mode-exclude-rules-list puppet-ts-mode-align-exclude-rules)

  ;; Treesitter
  (when (treesit-ready-p 'puppet)
    (treesit-parser-create 'puppet)

    ;; Font-Lock
    (setq-local treesit-font-lock-feature-list puppet-ts-mode-feature-list)
    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules
                       puppet-ts-mode-font-lock-settings))

    ;; Indentation
    (setq-local treesit-simple-indent-rules puppet-ts-indent-rules)

    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pp\\'" . puppet-ts-mode))

(provide 'puppet-ts-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; puppet-ts-mode.el ends here
