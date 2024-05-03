# Emacs major mode for Puppet using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml)

This is a major mode to edit Puppet manifests using a tree-sitter parser.

**Note:** This is still work in progress; many details concerning font-lock or indentation might not yet work as expected. Most convenience functions of the old puppet-mode are not (yet) implemented.

## Features

The following features are planned:

1. Syntax highlighting (*mostly done*)
1. Indentation and alignment of parameter lists, expressions and statements (*started*)
1. Cross-reference navigation (aka `xref`) to classes, defined types, data types or functions defined in other modules
1. Skeletons for many standard Puppet statements and resource declarations
1. Tag navigation (aka `imenu`)

## Installation

Emacs 29.1 or above with tree-sitter support is required.

The following Elisp code should be used to install the Puppet language parser.  This requires some tools -- notably a compiler toolchain -- to be available on your machine.

```elisp
(require 'puppet-ts-mode)
(puppet-ts-mode-install-grammar)
```

Using the function provided by the package ensures that a version of the parser matching the package will be installed.  These commands should also be used to update the parser to the corrent version when the package is updated.

## Supporting packages

Some other packages are useful to support your workflow when editing Puppet code.  While these packages are not integrated in `puppet-ts-mode`, they can be installed in addition if you like them.

Examples:

* *cycle-quotes* Cycle between quote styles
* *change-inner* Change contents based on semantic units

## License

Puppet Treesitter Mode is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Puppet Treesitter Mode is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
