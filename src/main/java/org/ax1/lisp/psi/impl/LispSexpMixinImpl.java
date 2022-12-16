package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispElementFactory;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.usages.LispSexpReference;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.Type.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.Type.VARIABLE;

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
  public SymbolBinding getSymbolDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getDefinition(this);
  }

  @Override
  public LispDefinition getDefinition() {
    SymbolBinding symbolDefinition = getSymbolDefinition();
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
    SymbolBinding symbolBinding = getSymbolDefinition();
    if (symbolBinding != null) {
      return symbolBinding.symbol.getQualifiedName();
    }
    return null;
  }

  @Override
  public boolean isFunctionCall() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == FUNCTION && symbolBinding.isUsage(this);
  }

  @Override
  public boolean isFunctionDefinition() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == FUNCTION && symbolBinding.isDefinition(this);
  }

  @Override
  public boolean isVariableReference() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == VARIABLE && symbolBinding.isUsage(this);
  }

  @Override
  public boolean isVariableDefinition() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == VARIABLE && symbolBinding.isDefinition(this);
  }

  @Override
  public boolean isLexicalDefinition() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.scope == LEXICAL && symbolBinding.isDefinition(this);
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    if (symbolBinding != null && symbolBinding.scope == LEXICAL && symbolBinding.container != null) {
      return new LocalSearchScope(symbolBinding.container);
    }
    return super.getUseScope();
  }

}
