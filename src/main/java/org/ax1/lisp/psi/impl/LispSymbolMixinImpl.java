package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNameIdentifierOwner;
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

public abstract class LispSymbolMixinImpl extends ASTWrapperPsiElement implements PsiNameIdentifierOwner, LispSymbol {

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
  public @Nullable PsiElement getNameIdentifier() {
    if (isVariableDefinition() || isFunctionDefinition()) {
      return this;
    }
    return null;
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
  public boolean isLetVariableName() {
    PsiElement container1 = getParent().getParent();
    if (!(container1 instanceof LispList)) return false;
    PsiElement container2 = container1.getParent().getParent();
    if (!(container2 instanceof LispList)) return false;
    LispList formLet = (LispList) container2;
    if (!formLet.isFormLet()) {
      PsiElement container3 = container2.getParent().getParent();
      if (!(container3 instanceof LispList)) return false;
      formLet = (LispList) container3;
      if (!formLet.isFormLet()) return false;
    }
    for (LispSexp sexp : formLet.getVariableList()) {
      if (sexp.getSymbol() == this) return true;
      LispList list = sexp.getList();
      if (list != null) {
        List<LispSexp> varInitList = list.getSexpList();
        if (!varInitList.isEmpty()) {
          LispSymbol symbol = varInitList.get(0).getSymbol();
          if (symbol == this) return true;
        }
      }
    }

    return false;
  }

  @Override
  public boolean isParameterName() {
    PsiElement container1 = getParent().getParent();
    if (!(container1 instanceof LispList)) return false;
    PsiElement container2 = container1.getParent().getParent();
    if (!(container2 instanceof LispList)) return false;
    LispList defun = (LispList) container2;
    if (!defun.isFormDefun()) return false;
    for (LispSexp sexp : defun.getDefunLambdaList()) {
      if (sexp.getSymbol() == this) return true;
    }
    return false;
  }

  public boolean isDestructuringBindVariableName() {
    LispList destructuringBind = getSymbolDefinition().container;
    LispSexp lispSexp = destructuringBind.getSexpList().get(0);
    return lispSexp.getSymbol().getText().equals("destructuring-bind");
  }

  @Override
  public LispList getDefunFromParameter() {
    LispList lambdaList = (LispList) getParent().getParent();
    return (LispList) lambdaList.getParent().getParent();
  }

  @Override
  public SymbolBinding getSymbolDefinition() {
    return ProjectComputedData.getInstance(getProject()).getProjectAnalysis().getDefinition(this);
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    SymbolBinding symbolBinding = getSymbolDefinition();
    if (symbolBinding != null && symbolBinding.scope == LEXICAL) {
      return new LocalSearchScope(symbolBinding.container);
    }
    return super.getUseScope();
  }
}