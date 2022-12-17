package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.usages.LispSexpReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.VARIABLE;

public abstract class LispSexpMixinImpl extends ASTWrapperPsiElement implements LispSexp {

  public LispSexpMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public PsiReference getReference() {
    LispDefinition definition = getDefinition();
    if (definition != null && definition.isUsage(this)) {
      return new LispSexpReference(this, definition.getDefinition());
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
    LispSexp newSexp = LispElementFactory.createSymbol(getProject(), newName);
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
  public boolean isFunctionCall() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.type == FUNCTION && symbolDefinition.isUsage(this);
  }

  @Override
  public boolean isFunctionDefinition() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.type == FUNCTION && symbolDefinition.isDefinition(this);
  }

  @Override
  public boolean isVariableReference() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.type == VARIABLE && symbolDefinition.isUsage(this);
  }

  @Override
  public boolean isVariableDefinition() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.type == VARIABLE && symbolDefinition.isDefinition(this);
  }

  @Override
  public boolean isLexicalDefinition() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    return symbolDefinition != null && symbolDefinition.scope == LEXICAL && symbolDefinition.isDefinition(this);
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    SymbolDefinition symbolDefinition = getSymbolDefinition();
    if (symbolDefinition != null && symbolDefinition.scope == LEXICAL && symbolDefinition.container != null) {
      return new LocalSearchScope(symbolDefinition.container);
    }
    return super.getUseScope();
  }

}
