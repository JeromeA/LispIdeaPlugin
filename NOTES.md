
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

To parse a file, we need to know everything about the packages they use.

From the point of view of multi-passes parsing, there are 3 categories of project:
* Trivial projects, that only use CL-USER package, or packages which in turn only use CL.
Super easy to parse in one single pass as we know everything about the packages, even if we didn't see them yet.
But they are just test projects, this never happens for real.
* Projects with packages. If we parse a file without knowing the package definition, we will have to parse it again.
In that case, it's better to have a quick pass to find the DEFPACKAGEs first (and skip everything else), so that the
next pass gets everything right.
* Project that mess with packages (like calling DEFPACKAGE from macros), but for these, we won't be able to avoid
multi-pass, unless we start taking system loading into account, which is not going to happen soon, and will always
be limited.

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

## Unknown package

When finding a symbol from an unknown package, we could either:
* Return a null Symbol, and the caller should mark it as invalid.
* Create a default package and proceed. Anyway, it will be marked later as undefined. No need to add the burden of
checking for null in every single symbol management.

I am going for the second solution for now. 

## Usage highlighting

Usage highlighting relies on many assumptions that a plugin can easily break. I broke it multiple times already,
and I pledge to write down the reasons whenever it happens again.

For the feature to work, the symbols have to provide a reference, and this reference must resolve to the
definition.

Summary:
* IdentifierHighlighterPass.highlightReferencesAndDeclarations looks up for any declaration or reference at the cursor 
position, by calling getTargetSymbols().
  * A declaration can be provided by:
    * A PsiSymbolDeclarationProvider
    * PsiElement.getOwnDeclarations() coupled with PsiElement.getOwnReferences().
    * NamedElement at cursor (even though it's not necessarily a declaration).
  * A reference is found by calling getReference on the element at the cursor position.
  * If we found both a declaration and a reference, we only keep the reference.
  * Finally getTargetSymbols() resolves the reference and returns the definition.
* Check if LispFindUsagesHandlerFactory can find usages for this PsiElement.
* Call LispFindUsagesHandler.findReferencesToHighlight().
* Finally, the definition is added to the list to highlight:
  * If it's a PsiNameIdentifierOwner, call getNameIdentifier().
  * Otherwise, see if a token is exactly the text of the symbol. This is the reason we are implementing
PsiNameIdentifierOwner, not only for packages (the string token includes quotes and escape characters) but also
for symbols (the token may contain the package name).

## Business code in PsiElement

A LispSymbol can be a variable name or a function name, and it can be either a definition or a usage.

However, the package management code can't be in LispSymbol, because the same package can be referenced
by either a symbol or a string.

My first solution was to have package management code at LispSexp level, which can contain either a symbol or a string.
Problem: MemberInplaceRenamer.performRefactoringRename() was skipping our LispSymbol when renaming a symbol, because
there was a parent (the LispSexp) that was also a PsiNameIdentifierOwner in the renaming range.

Then I switched to having all the code in LispSexp, for both symbol and package management. It was very convenient
to have a single class covering all the cases. Unfortunately, I hit another wall when trying to make renaming work:
having a package name and a symbol name in the same PsiElement means that there was no way to make renaming
and other symbol feature work properly. In PACKAGE:SYMBOL notation, it's important that the package and the symbol
are two separate PsiElement, each with its own independent behavior.

So, the new solution is to have the code at a node that exactly matches its name:
* the symbol management happens at the PsiElement that contains only the symbol name.
* the package management happens at the PsiElement that contains either the package name in a symbol, or the package
name in a string.

## Find usages

Find usages is completely unrelated to usage highlighting. It is based on LispFindUsagesProvider.

Summary:
* SearchForUsagesRunnable.searchUsages()
* call LowLevelSearchUtil.processElementsAtOffsets
* which calls processTreeUp to go through the token and its parents all the way to the File.
* they are checked one after another by calling SingleTargetRequestResultProcessor.processTextOccurrence.
* which looks up for any reference for that PsiElement
* if the reference resolves to the target, it's a hit

## In-place renaming

Several classes are involved:
* RenameElementAction. This is the main entry point, triggered by shift-F6
* VariableInplaceRenamer and MemberInplaceRenamer, but only of these two makes sense in Lisp. Which one?

In InplaceRefactoring.startTemplate():
* At the beginning of this method, the myEditor.myDocument contains the original text.
* Then the names are removed.
* Then the call to TemplateManager.getInstance(myProject).startTemplate() is inserting an uppercase version of the names.

When startTemplate() is called, myRenameOffset already contains the wrong range [8,25] instead of [16,25].
Unfortunately, the InplaceRefactoring constructor 