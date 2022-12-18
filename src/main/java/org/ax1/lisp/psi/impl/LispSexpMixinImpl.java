package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.ElementManipulators;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispTypes;
import org.ax1.lisp.usages.LispSexpReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Objects;
import java.util.stream.Stream;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.VARIABLE;

public abstract class LispSexpMixinImpl extends ASTWrapperPsiElement implements LispSexp {

  public LispSexpMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public PsiReference getReference() {
    return getSymbolReference();
  }

  @Override
  public PsiReference @NotNull [] getReferences() {
    return Stream.of(getSymbolReference(), getPackageReference()).filter(Objects::nonNull).toArray(PsiReference[]::new);
  }

  @Override
  public PsiReference getSymbolReference() {
    SymbolDefinition definition = getSymbolDefinition();
    if (definition != null && definition.isUsage(this)) {
      TextRange range = ElementManipulators.getValueTextRange(this);
      String text = getText();
      int colonIndex = text.indexOf(':');
      if (colonIndex >= 0) {
        range = TextRange.create(range.getStartOffset() + colonIndex + 1, range.getEndOffset());
      }
      return new LispSexpReference(this, definition.getDefinition(), range);
    }
    return null;
  }

  @Override
  public PsiReference getPackageReference() {
    PackageDefinition packageDefinition = getPackageDefinition();
    if (packageDefinition != null && packageDefinition.isUsage(this)) {
      TextRange range = ElementManipulators.getValueTextRange(this);
      if (isString()) {
        return new LispSexpReference(this, packageDefinition.getDefinition(),
            TextRange.create(range.getStartOffset() + 1, range.getEndOffset() - 1));
      }
      String text = getText();
      int colonIndex = text.indexOf(':');
      if (getSymbolDefinition() != null) {
        // Reference to the package part of a symbol.
        if (colonIndex <= 0 || text.startsWith("#:")) {
          throw new RuntimeException("No package part in symbol");
        }
        return new LispSexpReference(this, packageDefinition.getDefinition(),
            TextRange.create(range.getStartOffset(), range.getStartOffset() + colonIndex));
      }
      // Using a symbol as a string designator.
      return new LispSexpReference(this, packageDefinition.getDefinition(),
          TextRange.create(range.getStartOffset() + colonIndex + 1, range.getEndOffset()));
    }
    return null;
  }

  @Override
  public PackageDefinition getPackageDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getPackage(this);
  }

  @Override
  public SymbolDefinition getSymbolDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getDefinition(this);
  }

  @Override
  public LispDefinition getDefinition() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    if (symbolDefinition != null) return symbolDefinition;
    return getPackageDefinition();
  }

  @Override
  public PsiElement setName(@NotNull String newName) {
    String prefix = getText().substring(0, getSymbolNameOffset());
    LispSexp newSexp = LispElementFactory.createSymbol(getProject(), prefix + newName);
    ASTNode parent = getNode().getTreeParent();
    parent.replaceChild(getNode(), newSexp.getNode());
    return this;
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    return this;
  }

  @Override
  public String getName() {
    // TODO: find the exact upper/lower case combination used in the source.
    //   getName() is used as the starting point for in-place renaming, so we should make sure that the
    //   returned case matches the one used in the source. Here, we just take the lower case version, which is
    //   the most common pattern.
    LispDefinition lispDefinition = getDefinition();
    if (lispDefinition != null) {
      return lispDefinition.getName().toLowerCase();
    }
    return getText();
  }

  @Override
  public @Nullable String getQualifiedName() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    if (symbolDefinition != null) {
      return symbolDefinition.symbol.getQualifiedName();
    }
    return null;
  }

  @Override
  public boolean isFunctionDefinition() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.type == FUNCTION && symbolDefinition.isDefinition(this);
  }

  @Override
  public boolean isVariableDefinition() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.type == VARIABLE && symbolDefinition.isDefinition(this);
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    if (symbolDefinition != null && symbolDefinition.scope == LEXICAL && symbolDefinition.container != null) {
      return new LocalSearchScope(symbolDefinition.container);
    }
    return super.getUseScope();
  }

  public boolean isString() {
    PsiElement firstChild = getFirstChild();
    if (!(firstChild instanceof LeafPsiElement)) return false;
    LeafPsiElement leafFirstChild = (LeafPsiElement) firstChild;
    return leafFirstChild.getElementType() == LispTypes.STRING;
  }

  public boolean isSymbol() {
    return getSymbol() != null;
  }

  /*
   * getPsiUsageRanges() will only accept a modified range for the target (the declaration symbol) when that target
   * is a PsiNameIdentifierOwner, and the Identifier is implementing ExternallyAnnotated. So we're doing that.
   */
  @Override
  public @Nullable TextRange getAnnotationRegion() {
    TextRange range = getTextRange();
    if (isString()) {
      return TextRange.create(range.getStartOffset() + 1, range.getEndOffset() - 1);
    }
    if (isSymbol()) {
      return TextRange.create(range.getStartOffset() + getSymbolNameOffset(), range.getEndOffset());
    }
    return range;
  }

  private int getSymbolNameOffset() {
    String text = getText();
    int doubleColon = text.indexOf("::");
    if (doubleColon >= 0) return doubleColon + 2;
    int colon = text.indexOf(':');
    if (colon >= 0) return colon + 1;
    return 0;
  }
}
