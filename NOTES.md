
## Parsing

A few problems with the current implementation:
* The PackageManager mixes two roles: the parsing context (currentPackage, and logic to resolve symbols), and the 
reference package storage. The current package should be part of the parsing context, while the package map should
part of the project level information.
* The parsing is trying to be too smart. For example, whenever we see a function definition or a function usage, we
fetch the current function data structure, and add the new finding to that structure. This makes any future change
difficult to track precisely. We should just store the fact that we saw a definition of a function usage in this
file, and these pieces of information will be merged into a global project level at a later stage. This way,
when a file changes, we can just scratch its information without leaving any traces of the previous state.

The context should have:
* A highlighter, which can be a dummy one in case we're analyzing a closed file.
* A symbol resolver, which knows the current package, and the project ones.
* A SyntaxAnalyzer to call for analyzing sub forms.

A symbol always belongs to a package, we still have a lot of code that wrongly assumes that a symbol can be a String.

## Packages

I have trouble deciding between two modes:
* A package definition knows the other packages it relies on, but then it's a mess when a Package object has to
be rebuilt, or when we only have a partial set of packages parsed, as we still have to know the other packages by
name for some time.
* A package definition only knows other packages by name. This means that resolving symbols requires knowing the
PackageManager. And then there are two options again:
  * The package knows the PackageManager.
  * The resolve function is given the PackageManager on each call.

There could be reasons to pass the PackageManager on each call (if the PackageManager can change over time, of if we
need multiple PackageManager to co-exist), but I can't find a valid one at the moment, so the current decision is that
all packages know the PackageManager.

## Multiple passes for parsing

When it comes to the number of passes, there are 3 categories of project:
* Test projects, that only use CL-USER package, or packages which only use CL. Super easy to parse in one single pass.
But they are just test projects, this never happens for real.
* Projects with packages. If we parse a file without knowing the package definition, we will have to parse it again.
In that case, it's better to have a quick pass to find the DEFPACKAGEs first (and skip everything else), so that the
next pass gets everything right.
* Project that mess with packages (like calling DEFPACKAGE from macros), but for these, we won't be able to avoid
multi-pass, unless we start taking ASDF into account, which is not going to happen early enough.

So, we're optimizing for the second case: a quick pass is checking for DEFPACKAGEs before the real parsing is happening.

## Name resolution

The algorithm for name resolution should be:
* If the symbol is specified as PACKAGE:NAME, use findExportedSymbol() in that package, and check it's exported.
* If the symbol is specified as PACKAGE::NAME, use findSymbol() in that package, which may do recursive calls into other
packages;
* Otherwise, use intern() on the default package.

findExportedSymbol() only lookup the set of exported symbols.

findSymbol():
* searches the local map (which already contains the shadows).
* if not found, calls findExportedSymbols() on the uses.
* if not found, search the importFrom data structure.
* if not found, return nil.

intern() calls findSymbol(), and create the symbol if nothing is found.

## Cleanup

In the current CL, I have to do the following cleanups:
* remove any reference to SymbolBinding
