;;; test-helper.el --- unit-test setup -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(message "Loaded %s" (file-relative-name load-file-name))

(message "Running tests on Emacs %s" emacs-version)

(let* ((cur (if load-in-progress load-file-name buffer-file-name))
       (dir (locate-dominating-file cur "Cask")))
  (load (expand-file-name "puppet-ts-mode" dir)))

;;; test-helper.el ends here
