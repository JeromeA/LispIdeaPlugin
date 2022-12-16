package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.ProjectComputedData;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.usages.LispSymbolReference;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.ax1.lisp.analysis.SymbolBinding.Scope.LEXICAL;
import static org.ax1.lisp.analysis.SymbolBinding.Type.FUNCTION;
import static org.ax1.lisp.analysis.SymbolBinding.Type.VARIABLE;

public abstract class LispSymbolMixinImpl extends ASTWrapperPsiElement implements LispSymbol {

  public LispSymbolMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  @Override
  public PsiReference getReference() {
    if (isVariableReference() || isFunctionCall()) {
      List<LispSymbol> definitions = getSymbolDefinition().definitions;
      if (definitions.isEmpty()) return null;
      return new LispSymbolReference(this, definitions.get(0));
    }
    return null;
  }

  @Override
  public String getName() {
    // TODO: find the exact upper/lower case combination used in the source.
    //   getName() is used as the starting point for in-place renaming, so we should make sure that the
    //   returned case matches the one used in the source. Here, we just take the lower case version, which is
    //   the most common pattern.
    SymbolBinding symbolBinding = getSymbolDefinition();
    if (symbolBinding != null) {
      return symbolBinding.symbol.getName().toLowerCase();
    }
    return getText();
  }

  @Override
  public PsiElement setName(@NotNull String newName) {
    LispSymbol newSymbol = LispElementFactory.createSymbol(getProject(), newName);
    ASTNode parent = getNode().getTreeParent();
    parent.replaceChild(getNode(), newSymbol.getNode());
    return this;
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
  public @Nullable PsiElement getNameIdentifier() {
    return this;
  }

  @Override
  public boolean isFunctionCall() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == FUNCTION && symbolBinding.usages.contains(this);
  }

  @Override
  public boolean isFunctionDefinition() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == FUNCTION && symbolBinding.definitions.contains(this);
  }

  @Override
  public boolean isVariableReference() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == VARIABLE && symbolBinding.usages.contains(this);
  }

  @Override
  public boolean isVariableDefinition() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.type == VARIABLE && symbolBinding.definitions.contains(this);
  }

  @Override
  public boolean isLexicalDefinition() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    return symbolBinding != null && symbolBinding.scope == LEXICAL && symbolBinding.definitions.contains(this);
  }

  @Override
  public SymbolBinding getSymbolDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getDefinition(this);
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