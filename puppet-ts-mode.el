;;; puppet-ts-mode.el --- Major mode for Puppet using Tree-sitter -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Stefan Möding

;; Author:           Stefan Möding <stm@kill-9.net>
;; Maintainer:       Stefan Möding <stm@kill-9.net>
;; Version:          0.1.0
;; Created:          <2024-03-02 13:05:03 stm>
;; Updated:          <2024-04-30 07:50:54 stm>
;; URL:              https://github.com/smoeding/puppet-ts-mode
;; Keywords:         languages, puppet, tree-sitter
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
(require 'xref)

(eval-when-compile
  (require 'cl-lib)
  (require 'rx))


;;; Customization

(defgroup puppet-ts nil
  "Write Puppet manifests in Emacs."
  :prefix "puppet-ts-"
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

(defvar puppet--statement-functions
  '("include" "require" "contain" "tag"            ; Catalog statements
    "debug" "info" "notice" "warning" "err" "crit" ; Logging statements
    "fail")                                        ; Failure statements
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
                      ,(cons 'or (append puppet--builtin-functions
                                         puppet--statement-functions))
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
  :group 'puppet-ts)

(defface puppet-string-face
  '((t :inherit font-lock-string-face))
  "Face for strings in Puppet."
  :group 'puppet-ts)

(defface puppet-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for keywords in Puppet."
  :group 'puppet-ts)

(defface puppet-resource-type-face
  '((t :inherit font-lock-type-face))
  "Face for resource types in Puppet."
  :group 'puppet-ts)

(defface puppet-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for built-in functions in Puppet."
  :group 'puppet-ts)

(defface puppet-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for a constant in Puppet."
  :group 'puppet-ts)

(defface puppet-variable-name-face
  '((t :inherit font-lock-variable-name-face))
  "Face for the name of a variable in Puppet."
  :group 'puppet-ts)

(defface puppet-variable-use-face
  '((t :inherit font-lock-variable-use-face))
  "Face for the name of a variable being referenced in Puppet."
  :group 'puppet-ts)

(defface puppet-function-name-face
  '((t :inherit font-lock-function-name-face))
  "Face for the name of a function in Puppet."
  :group 'puppet-ts)

(defface puppet-function-call-face
  '((t :inherit font-lock-function-call-face))
  "Face for the name of a function being called in Puppet."
  :group 'puppet-ts)

(defface puppet-operator-face
  '((t :inherit font-lock-operator-face))
  "Face for operators."
  :group 'puppet-ts)

(defface puppet-negation-char-face
  '((t :inherit font-lock-negation-char-face))
  "Face for negation characters."
  :group 'puppet-ts)

(defface puppet-number-face
  '((t :inherit font-lock-number-face))
  "Face for numbers."
  :group 'puppet-ts)

(defface puppet-warning-face
  '((t :inherit font-lock-warning-face))
  "Face for language errors found by the parser."
  :group 'puppet-ts)


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
    (keyword resource-type string)
    (constant number variable interpolation builtin function)
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

    :feature variable
    :language puppet
    ((variable ["$" (name)] @puppet-variable-name-face))

    :feature constant
    :language puppet
    (((true) @puppet-constant-face)
     ((false) @puppet-constant-face)
     ((default) @puppet-constant-face)
     ((undef) @puppet-constant-face))

    :feature number
    :language puppet
    ((number) @puppet-number-face)

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
     ((case "case" @puppet-keyword-face))
     (binary operator: ["and" "or" "in"] @puppet-keyword-face))

    :feature resource-type
    :language puppet
    (((resource_type [(name) (virtual) (exported)] @puppet-resource-type-face))
     ;; arguments to statement functions
     ((statement_function
       (argument_list (argument (name) @puppet-resource-type-face))))
     ;; data and resource reference types
     ((type) @puppet-resource-type-face))

    :feature operator
    :language puppet
    ((unary operator: "!" @puppet-negation-char-face)
     (unary operator: _  @puppet-operator-face)
     (binary operator: _ @puppet-operator-face))

    :feature error
    :language puppet
    :override t
    ((ERROR) @puppet-warning-face))
  "`treesit-font-lock-settings' for `puppet-ts-mode'.")


;; Indentation

(defcustom puppet-indent-level 2
  "Number of spaces for each indententation step."
  :group 'puppet-ts
  :type 'integer
  :safe 'integerp)

(defcustom puppet-indent-tabs-mode nil
  "Indentation can insert tabs in puppet mode if this is non-nil."
  :group 'puppet-ts
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
Return the beginning of line position for the Puppet definition.

The signature of this function is defined by Tree-Sitter."
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
Return the beginning of line position for the Puppet resource.

The signature of this function is defined by Tree-Sitter."
  (let ((ancestor (puppet-find-ancestor-node
                   (or node parent)     ; Start with parent if node is nil
                   (regexp-opt '("resource_type" "resource_reference")))))
    (if ancestor
        (save-excursion
          (goto-char (treesit-node-start ancestor))
          (back-to-indentation)
          (point)))))

(defun puppet-ancestor-function-bol (node parent _bol)
  "Search ancestors of NODE or PARENT for a Puppet function call.

The search starts with PARENT if NODE is NIL.  This happens if no
node can start at the position, e.g. there is an empty line.
Return the beginning of line position for the Puppet resource.

The signature of this function is defined by Tree-Sitter."
  (let ((ancestor (puppet-find-ancestor-node
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
              (list (cons 'definition-bol #'puppet-ancestor-definition-bol)
                    (cons 'resource-bol #'puppet-ancestor-resource-bol)
                    (cons 'function-bol #'puppet-ancestor-function-bol))))

(defvar puppet-ts-indent-rules
  `((puppet
     ;; top-level statements start in column zero
     ((parent-is "manifest") column-0 0)
     ;; blocks
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "block") parent-bol puppet-indent-level)
     ;; compound statements
     ((node-is "elsif") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ;; arrays
     ((match "array_element" nil nil 1 1) parent-bol puppet-indent-level)
     ((match "array_element" nil nil 2 nil) prev-sibling 0)
     ((parent-is "array") parent-bol puppet-indent-level)
     ;; structures and expressions
     ((parent-is "case") parent-bol puppet-indent-level)
     ((parent-is "hash") parent-bol puppet-indent-level)
     ((parent-is "selector") parent-bol puppet-indent-level)
     ((parent-is "function_call") parent-bol puppet-indent-level)
     ((parent-is "resource_type") resource-bol puppet-indent-level)
     ((parent-is "resource_body") resource-bol puppet-indent-level)
     ((parent-is "resource_reference") resource-bol puppet-indent-level)
     ((node-is "attribute") resource-bol puppet-indent-level)
     ((parent-is "attribute_list") resource-bol puppet-indent-level)
     ((parent-is "parameter_list") definition-bol puppet-indent-level)
     ((parent-is "argument_list") function-bol puppet-indent-level)
     (catch-all parent-bol 0)))
  "Indentation rules for `puppet-ts-mode'.")


;; Xref

(defcustom puppet-module-path
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

(defun puppet-module-root (file)
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

(defun puppet-autoload-path (identifier &optional directory extension)
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

(defun puppet--xref-backend ()
  "The Xref backend for `puppet-ts-mode'."
  'puppet)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql puppet)))
  "Return the Puppet identifier at point."
  (let ((thing (thing-at-point 'symbol)))
    (and thing (substring-no-properties thing))))

(cl-defmethod xref-backend-definitions ((_backend (eql puppet)) identifier)
  "Find the definitions of a Puppet resource IDENTIFIER.

First the location of the visited file is checked.  Then all
directories from `puppet-module-path' are searched for the module
and the file according to Puppet's autoloading rules."
  (let* ((resource (downcase (if (string-prefix-p "::" identifier)
                                 (substring identifier 2)
                               identifier)))
         (pupfiles (puppet-autoload-path resource))
         (typfiles (puppet-autoload-path resource "types"))
         (funfiles (puppet-autoload-path resource "functions"))
         (xrefs '()))
    (if pupfiles
        (let* ((module (car pupfiles))
               ;; merged list of relative path names to classes/defines/types
               (pathlist (mapcar #'cdr (list pupfiles typfiles funfiles)))
               ;; list of directories where this module might be
               (moddirs (mapcar (lambda (dir) (expand-file-name module dir))
                                puppet-module-path))
               ;; the regexp to find the resource definition in the file
               (resdef (concat "^\\(class\\|define\\|type\\|function\\)\\s-+"
                               resource
                               "\\((\\|{\\|\\s-\\|$\\)"))
               ;; files to visit when searching for the resource
               (files '()))
          ;; Check the current module directory (if the buffer actually visits
          ;; a file) and all module subdirectories from `puppet-module-path'.
          (dolist (dir (if buffer-file-name
                           (cons (puppet-module-root buffer-file-name) moddirs)
                         moddirs))
            ;; Try all relative path names below the module directory that
            ;; might contain the resource; save the file name if the file
            ;; exists and we haven't seen it (we might try to check a file
            ;; twice if the current module is also below one of the dirs in
            ;; `puppet-module-path').
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

The mode supports the cross-referencing system documented in the
Info node `Xref'.  The customization variable `puppet-module-path'
contains a list of directories that are searched to find locally
installed Puppet modules.

Calling the function `xref-find-definitions' (bound to \\[xref-find-definitions])
with point on an identifier \(a Puppet class, defined type,
custom function or data type) jumps to the definition of that
identifier.

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

  ;; Xref
  (add-hook 'xref-backend-functions #'puppet--xref-backend)

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
