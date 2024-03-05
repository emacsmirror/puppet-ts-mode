# Puppet major mode using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This is puppet-ts-mode, a major mode to edit Puppet files using the tree-sitter parser for Puppet.

**Caution:** This is work in progress; many details concerning font-lock or indentation might not yet work as expected.  Most convenience functions of the old puppet-mode are not (yet) implemented.

## Installation

Emacs 29.1 or above with tree-sitter support is required.

The following Elisp code can be used to add the Puppet language parser to your Emacs setup.

```elisp
(add-to-list 'treesit-language-source-alist
             '(puppet "https://github.com/tree-sitter-grammars/tree-sitter-puppet"))

(treesit-install-language-grammar 'puppet)
```

This requires some tools, notably a compiler toolchain, to be available on your machine. See the Tree-sitter starter guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
