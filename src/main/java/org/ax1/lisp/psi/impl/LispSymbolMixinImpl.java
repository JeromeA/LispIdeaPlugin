package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.analysis.ProjectAnalyser;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.usages.LispSymbolReference;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

public abstract class LispSymbolMixinImpl extends ASTWrapperPsiElement implements PsiNameIdentifierOwner, LispSymbol {

  public LispSymbolMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  public PsiReference getReference() {
    if (isVariableReference() || isFunctionCall()) {
      LispSymbol definition = getSymbolBinding().getDefinition();
      return definition == null ? null : new LispSymbolReference(this, definition);
    }
    if (isPackageReference()) {
      LispSymbol definition = getPackageDefinition().getDefinition();
      return definition == null ? null : new LispSymbolReference(this, definition);
    }
    return null;
  }

  private boolean isPackageReference() {
    PackageDefinition packageDefinition = getPackageDefinition();
    return packageDefinition != null && packageDefinition.getUsages().contains(this);
  }

  private boolean isPackageDefinition() {
    PackageDefinition packageDefinition = getPackageDefinition();
    return packageDefinition != null && packageDefinition.getDefinition() == this;
  }

  @Override
  public String getName() {
    return getText();
  }

  public PsiElement setName(String newName) {
    LispSymbol newSymbol = LispElementFactory.createSymbol(getProject(), newName);
    ASTNode parent = getNode().getTreeParent();
    parent.replaceChild(getNode(), newSymbol.getNode());
    return this;
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    if (isVariableDefinition() || isFunctionDefinition() || isPackageDefinition()) {
      return this;
    }
    return null;
  }

  public PackageDefinition getPackageDefinition() {
    return ProjectAnalyser.getInstance(getProject()).getProjectSymbolAnalysis().packages.get(this);
  }

  @Override
  public boolean isFunctionCall() {
    SymbolBinding symbolBinding = getSymbolBinding();
    return symbolBinding != null && symbolBinding.getSymbolType() == FUNCTION && symbolBinding.getDefinition() != this;
  }

  @Override
  public boolean isFunctionDefinition() {
    SymbolBinding symbolBinding = getSymbolBinding();
    return symbolBinding != null && symbolBinding.getSymbolType() == FUNCTION && symbolBinding.getDefinition() == this;
  }

  @Override
  public boolean isVariableReference() {
    SymbolBinding symbolBinding = getSymbolBinding();
    return symbolBinding != null && symbolBinding.getSymbolType() == VARIABLE && symbolBinding.getDefinition() != this;
  }

  @Override
  public boolean isVariableDefinition() {
    SymbolBinding symbolBinding = getSymbolBinding();
    return symbolBinding != null && symbolBinding.getSymbolType() == VARIABLE && symbolBinding.getDefinition() == this;
  }

  @Override
  public boolean isLexicalDefinition() {
    SymbolBinding symbolBinding = getSymbolBinding();
    return symbolBinding != null && symbolBinding.getBindingType() == LEXICAL && symbolBinding.getDefinition() == this;
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
    LispList destructuringBind = getSymbolBinding().getContainer();
    LispSexp lispSexp = destructuringBind.getSexpList().get(0);
    return lispSexp.getSymbol().getText().equals("destructuring-bind");
  }

  @Override
  public LispList getDefunFromParameter() {
    LispList lambdaList = (LispList) getParent().getParent();
    return (LispList) lambdaList.getParent().getParent();
  }

  @Override
  public SymbolBinding getSymbolBinding() {
    return ProjectAnalyser.getInstance(getProject()).getProjectSymbolAnalysis().bindings.get(this);
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    SymbolBinding symbolBinding = getSymbolBinding();
    if (symbolBinding != null && symbolBinding.getBindingType() == LEXICAL && symbolBinding.getContainer() != null) {
      return new LocalSearchScope(symbolBinding.getContainer());
    }
    return super.getUseScope();
  }
}