;;; test-helper.el --- unit-test setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(message "Running on Emacs %s" emacs-version)

(require 'ert)

;;;; Install & load grammar

(require 'treesit)

(add-to-list
 'treesit-language-source-alist
 '(puppet "https://github.com/tree-sitter-grammars/tree-sitter-puppet"))

(treesit-install-language-grammar 'puppet)

;;;; Load puppet-ts-mode

(let* ((cur (if load-in-progress load-file-name buffer-file-name))
       (dir (locate-dominating-file cur "Cask")))
  (load (expand-file-name "puppet-ts-mode" dir)))

;;;; Utility functions

(defmacro puppet-test-with-temp-buffer (content &rest body)
  "Evaluate BODY in a temporary buffer with CONTENT."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (puppet-ts-mode)
     (goto-char (point-min))
     ,@body))

(defun puppet-test-face-at (pos &optional content)
  "Get the face at POS in CONTENT.
If CONTENT is not given, return the face at POS in the current buffer."
  (if content
      (puppet-test-with-temp-buffer content
        (get-text-property pos 'face))
    (get-text-property pos 'face)))

(defconst puppet-test-syntax-classes
  [whitespace punctuation word symbol open-paren close-paren expression-prefix
              string-quote paired-delim escape character-quote comment-start
              comment-end inherit generic-comment generic-string]
  "Readable symbols for syntax classes.
Each symbol in this vector corresponding to the syntax code of
its index.")

(defun puppet-test-syntax-at (pos)
  "Get the syntax at POS.
Get the syntax class symbol at POS, or nil if there is no syntax a POS."
  (let ((code (syntax-class (syntax-after pos))))
    (aref puppet-test-syntax-classes code)))

(defun puppet-test-indent (code)
  "Test indentation of Puppet CODE.
The CODE argument is a string that should contain correctly
indented Puppet code.  The code is indented using `indent-region'
and the test succeeds if the result did not change"
  (puppet-test-with-temp-buffer code
                                (indent-region (point-min) (point-max))
                                (should (string= (buffer-string) code))))

;;; test-helper.el ends here
