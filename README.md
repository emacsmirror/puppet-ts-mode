# Emacs major mode for Puppet using tree-sitter

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml)

This is a major mode to edit Puppet manifests using a tree-sitter parser. It is a rewrite of the original [Puppet mode for Emacs](https://github.com/voxpupuli/puppet-mode)

**Note:** This is still work in progress; many details concerning font-lock or indentation might not yet work as expected. Most convenience functions of the old puppet-mode are not (yet) implemented.

## Features

The following features are planned for this mode.

### Syntax highlighting

*mostly done...*

### Indentation and alignment of parameter lists, expressions and statements

*started...*

### Cross-reference navigation

Navigation from a referenced class, defined type, data type or function to the file where this corresponding type is defined (aka `xref`) is fully implemented.

**Example:** Let's say you are editing the following `acme` class:

``` puppet
# a class managing everything
class acme (
  Stdlib::Absolutepath $www_root,
  Boolean              $enable,
) inherits acme::params {

  include nginx

  nginx::resource::server { 'test2.local':
    ensure   => stdlib::ensure($enable),
    www_root => $www_root,
    require  => Class['postgresql::server'],
  }
}
```

The following navigation options are available:

1. Point is on the `Stdlib::Absolutepath` data type: `M-.` opens the data type definition
1. Point is on the `acme::params` subclass: `M-.` opens the class definition
1. Point is on the `nginx` class: `M-.` opens the class definition
1. Point is on the `nginx::resource::server` defined type: `M-.` opens the type definition
1. Point is on the `stdlib::ensure` function: `M-.` opens the function definition
1. Point is on the `postgresql::server` string: `M-.` opens the class definition

Navigation to foreign classes only needs a list of directories to search (see the customization option `puppet-ts-module-path`). After jumping to a definition you can return back using `M-,`. These keybindings are defined by the `xref` package.

### Skeletons for many standard Puppet statements and resource declarations

If you are not using `yasnipet` or another template package, you can use the implemented skeletons to insert often used types and keywords.

**Example:** Typing <kbd>C-c C-t p</kbd> will insert the following snippet leaving point just before the colon:

``` puppet
package { :
  ensure => present,
}
```

#### Skeletons for types

Key                  | Skeleton
---------------------|---------------
<kbd>C-c C-t a</kbd> | type `anchor`
<kbd>C-c C-t c</kbd> | type `class`
<kbd>C-c C-t e</kbd> | type `exec`
<kbd>C-c C-t f</kbd> | type `file`
<kbd>C-c C-t g</kbd> | type `group`
<kbd>C-c C-t h</kbd> | type `host`
<kbd>C-c C-t n</kbd> | type `notify`
<kbd>C-c C-t p</kbd> | type `package`
<kbd>C-c C-t s</kbd> | type `service`
<kbd>C-c C-t u</kbd> | type `user`

#### Skeletons for keywords

Key                  | Skeleton
---------------------|-------------------
<kbd>C-c C-k c</kbd> | keyword `class`
<kbd>C-c C-k d</kbd> | keyword `define`
<kbd>C-c C-k n</kbd> | keyword `node`
<kbd>C-c C-k i</kbd> | keyword `if`
<kbd>C-c C-k e</kbd> | keyword `elsif`
<kbd>C-c C-k o</kbd> | keyword `else`
<kbd>C-c C-k u</kbd> | keyword `unless`
<kbd>C-c C-k s</kbd> | keyword `case`
<kbd>C-c C-k ?</kbd> | keyword `selector`

### Tag navigation

(aka `imenu`)

*not started...*

## Installation

Emacs 29.1 or above with tree-sitter support is required.

The following Elisp code should be used to install the Puppet language parser.  This requires some tools -- notably a compiler toolchain -- to be available on your machine.

```elisp
(require 'puppet-ts-mode)
(puppet-ts-mode-install-grammar)
```

Using the function provided by the package ensures that a version of the parser matching the package will be installed. These commands should also be used to update the parser to the corrent version when the package is updated.

## License

Puppet Treesitter Mode is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Puppet Treesitter Mode is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
