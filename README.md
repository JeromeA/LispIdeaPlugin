# LispIdeaPlugin

An IntelliJ plugin to support Common Lisp.

## Approach

This plugin tries to support Common Lisp through static analysis. It's more work, but it allows better real-time
features:

* As soon as a name is changed, all the references and usages are updated in real time.
* As soon as anything changes, it knows exactly what information just became stale.
* It knows about completions even in an incomplete SEXP.
* It can display modern IDE information in real time. Showing error messages in the code, or highlighting unused
variables requires knowing what changes in the file can invalidate this information.

A much more advanced plugin for Common Lisp is [SLT](https://github.com/Enerccio/SLT). One of the reasons it's more
advanced is because it doesn't need to be that smart: it just queries SWANK on demand.

## Limitations

* It's still at a very early development stage. It's unlikely to work on anything but my own personal projects.
* It doesn't support any package or system manager.
* It doesn't work well with macros that are not part of Common Lisp.
* It doesn't have a debugger.

## Installing

The plugin is [[live in Jetbrain Marketplace|https://plugins.jetbrains.com/plugin/24556-common-lisp/edit/versions/alpha]]
however the marketplace Alpha channel is not very visible from Intellij. Here are detailed instructions:

* Go to `Settings -> Plugins -> Manage Plugin Repositories`.
* Add `https://plugins.jetbrains.com/plugins/alpha/list` to the list of plugin repositories.
* In the Marketplace, search for `Common Lisp`.
* Click on `Install`.

## Features

### Navigating code

#### Highlighting

When the cursor is on a symbol, all the references to that symbol are highlighted, taking into account what the symbol
is (variable, function, package, etc.) and where it is defined.

#### Go to definition

Control+click on a symbol usage to go to its definition. This is still true when the definition is in another file,
when it's defined in a macro (like a for loop), and when the sexp defining the symbol is still incomplete.

#### Find usages

Find all the usages of a symbol, taking into account what the symbol is used for (variable, function, etc), its
package, and its export status.

#### Error highlighting

For everything that the plugin can check, it will highlight errors in the code. This includes:

* undefined symbol
* invalid number of arguments
* invalid syntax (for standard functions and macros)

#### Function documentation

### Editing code

#### Completion

Control+space to complete a symbol. The completion list is based the context in which it's used (variable, function, etc).

#### Renaming

shift+F6 to rename a symbol. This will rename all the references to that symbol, taking into account what the symbol is
(variable, function, etc), and where it is defined.

#### Formatting

ctrl+alt+L to format the code. This will indent the code, and add or remove newlines where necessary.

Identation is taking into account the sexp structure, but also the context in which the sexp is used (function call,
macro call, etc).

### Running code

The current top level sexp can be run by pressing ctrl+shift+F10, or by clicking the run button from the gutter.

