# Emacs major mode for Puppet using Tree-sitter

[![MELPA](https://melpa.org/packages/puppet-ts-mode-badge.svg)](https://melpa.org/#/puppet-ts-mode)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Build Status](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml/badge.svg)](https://github.com/smoeding/puppet-ts-mode/actions/workflows/CI.yaml)

This is a major mode for [GNU Emacs](https://www.gnu.org/software/emacs/) 29.1 or later which adds support for the [Puppet](https://www.puppet.com) domain-specific language. It uses a Tree-sitter parser for Puppet to be able to parse the code and provide fontification, indentation, navigation and more.

It is a rewrite of the original [Puppet mode for Emacs](https://github.com/voxpupuli/puppet-mode).

## Features

The mode provides the following features and enhancements to make writing Puppet manifests easier.

### Syntax highlighting

Syntax highlighting for the following elements is fully implemented:

- comments
- strings
- numbers
- variables
- constants (`true`, `false`, `default`, `undef`)
- keywords (`if`, `unless`, `case`, `and`, `or`, `in`, ...)
- resource types and metaparameters (`ensure`, `require`, `notify`, ...)
- definitions (classes, defined types, type aliases, functions, nodes, plans)
- built-in functions
- custom functions
- operators
- syntax errors

### Indentation and alignment

The alignment of resource attributes, function parameter lists and hashes is fully implemented. If point is within such a block, the function `puppet-ts-align-block` (bound to <kbd>C-c C-a</kbd> by default) will align the attributes and parameters with respect to the `=>` symbols.

**Example:** Consider the following badly written file resource:

``` puppet
file { '/var/www':
  ensure    => directory,
  owner =>     'root',
  group   =>'www-data',
  mode=>'0644',
}
```

Type <kbd>C-c C-a</kbd> to align all the attributes and fat arrows when point is somewhere in this resource. The result will be like this:

``` puppet
file { '/var/www':
  ensure => directory,
  owner  => 'root',
  group  => 'www-data',
  mode   => '0644',
}
```

### Navigation

The keybindings <kbd>C-M-a</kbd> and <kbd>C-M-e</kbd> jump to preceding or following resource declaration respectively. This seems to be more useful than jumping to the beginning or end of a definition since a Puppet manifest normally only has a single definition.

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

## Installation

Emacs 29.1 or above with Tree-sitter support is required.

The following Elisp code should be used to install the Puppet language parser.  This requires some tools -- notably a compiler toolchain -- to be available on your machine.

```elisp
(require 'puppet-ts-mode)
(puppet-ts-mode-install-grammar)
```

Using the function provided by the package ensures that a version of the parser matching the package will be installed. These commands should also be used to update the parser to the corrent version when the package is updated.

## License

Puppet Tree-sitter Mode is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Puppet Tree-sitter Mode is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
