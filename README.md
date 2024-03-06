# Emacs major mode for Puppet using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml)

This is puppet-ts-mode, a major mode to edit Puppet manifests using the tree-sitter parser for Puppet.

**Note:** This is work in progress; many details concerning font-lock or indentation might not yet work as expected.  Most convenience functions of the old puppet-mode are not (yet) implemented.

## Installation

Emacs 29.1 or above with tree-sitter support is required.

The following Elisp code can be used to add the Puppet language parser to your Emacs setup.

```elisp
(add-to-list 'treesit-language-source-alist
             '(puppet "https://github.com/tree-sitter-grammars/tree-sitter-puppet"))
```

Then install the parser for the Puppet language using the following command. This requires some tools-- notably a compiler toolchain -- to be available on your machine.

```elisp
(treesit-install-language-grammar 'puppet)
```

This command can also be used to update the parser to the latest version from time to time.

See the Tree-sitter starter guide: https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=emacs-29
