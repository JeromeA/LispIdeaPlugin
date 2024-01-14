# Dev notes

## Multiple passes for parsing

To parse a file, we need to know everything about the packages being used in that file. But to know all the packages
of a project, we need to parse all the files.

From the point of view of multi-passes parsing, there are 3 categories of projects:
* Trivial projects, that only use CL-USER package, or packages which in turn only use the CL package.
Super easy to parse in one single pass as we know everything about the packages, even if we didn't see them yet.
But they are just test projects, this never happens for real.
* Projects with packages. If we parse a file without knowing the package definition, we will have to parse it again.
In that case, it's better to have a quick pass to find the DEFPACKAGEs first (and skip everything else), so that the
next pass gets everything right.
* Project that mess with packages (like calling DEFPACKAGE from macros), but for these, we won't be able to avoid
multi-pass, unless we start taking system loading into account, which is not going to happen soon, and will always
be limited.

So, we're optimizing for the second case: a quick pass is checking for DEFPACKAGEs before the real parsing is happening.

## Name/Package resolution

The algorithm for name resolution is:
* If the symbol is specified without package, resolve it as if it was specified as \*CURRENTPACKAGE\*::S
* If the symbol is specified as P:S, it has to be EXPORTed from P, otherwise it's an error.
* if P IMPORTs S from Q, intern for Q::S
* check if any of the USEd package U, find-symbol for U:S.
* otherwise, return P:S.

## Unknown package

When finding a symbol from an unknown package, we could either:
* Mark it as invalid.
* Behave as if it's a simple package (no export, USEs CL).

The second solution is chosen, as it's very likely to be the correct behavior. 

## Usage highlighting

Usage highlighting relies on many assumptions that a plugin can easily break. I broke it multiple times already,
and I pledge to write down the reasons here whenever it happens again.

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

The first solution was to have symbol management code at LispSymbol level, and package management code at LispSexp level,
so that it can contain either a symbol or a string.
Problem: MemberInplaceRenamer.performRefactoringRename() was skipping our LispSymbol when renaming a symbol, because
there was a parent (the LispSexp) that was also a PsiNameIdentifierOwner in the renaming range.

The second solution was to have all the code in LispSexp, for both symbol and package management. It was very convenient
to have a single class covering all the cases. Unfortunately, I hit another wall when trying to make renaming work:
having a package name and a symbol name in the same PsiElement means that there was no way to make renaming
and other symbol feature work properly. In PACKAGE:SYMBOL notation, it's important that the package and the symbol
are two separate PsiElement, each with its own independent behavior.

So, the new solution is to have the code at a node that exactly matches its name. The code is located in
LispStringDesignatorImpl interface, which is a Mixin added to:
* SYMBOL_NAME, which is the symbol part in package:symbol syntax, or the whole symbol if there is no package prefix.
* PACKAGE_PREFIX, which is the package part in package:symbol syntax.
* STRING_CONTENT, which can contain a package, and occasionally a symbol (like in the export section of a DEFPACKAGE).
This way, the code or package management happens at the PsiElement that contains exactly the token
which is either a package name or a symbol name.

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
* VariableInplaceRenamer has several limitations, one of them is that it reverts to a dialog as soon as:
  * one of the references is in a different file (see InplaceRefactoring.performInplaceRefactoring()).
  * or the scope is not a local one (same place).
* MemberInplaceRenamer is the one we want to use. But MemberInplaceRenameHandler.isAvailable() will only work on
  PsiNameIdentifierOwner instances. This is the reason why the LispStringDesignator mixin extends
  PsiNameIdentifierOwner.

In InplaceRefactoring.startTemplate():
* At the beginning of this method, the myEditor.myDocument contains the original text.
* Then the names are removed.
* Then the call to TemplateManager.getInstance(myProject).startTemplate() is inserting the getName() version of the
names, which is why getName() should not return the upper case version, as canonical as it is.

## Exported symbols

On a real Lisp system, exported symbols are interned as soon as the package is created. But this process
is order dependent: are we re-exporting this symbol that comes from the exported list of a used package,
or should we create it locally? This works because the used packages were declared first, so that when
the package is created, we know the exact origin (and the symbol instance) for each exported name.

We want to reach the same behavior in our order-independent framework. We don't try to resolve any exported
name at package creation time, IMPORT-FROM and USED are just strings.

Whenever findSymbol() is called:
* we first look in the internal map, which is both a local symbol directory and a local cache.
* if it's not there, we look at our IMPORT-FROM, and if relevant ask the package for the symbol
* if it's not there, we ask each of USES if they know about it
* if it's not there, we proved that it's not a re-export
  * if we export it, we create it on the fly and return it.
  * otherwise, it's really unknown, and it will only be created if we are called from intern.

At each step, if we found a symbol, we store it in the internal map for later optimization.

# Indexes

The following indexes are used, every time a non-lexical symbol is queried:
* Package definition
* Function/Generic definitions
* Method definitions
* Global variable definitions
* Package usages
* Function usages
* variable usages
