;;; test-helper.el --- unit-test setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(message "Running on Emacs %s" emacs-version)

(require 'treesit)

(let* ((cur (if load-in-progress load-file-name buffer-file-name))
       (dir (locate-dominating-file cur "Cask")))
  (load (expand-file-name "puppet-ts-mode" dir)))

(add-to-list 'treesit-language-source-alist
             '(puppet "https://github.com/tree-sitter-grammars/tree-sitter-puppet"))

(treesit-install-language-grammar 'puppet)

;;; test-helper.el ends here
