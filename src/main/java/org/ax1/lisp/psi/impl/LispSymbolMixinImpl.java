package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.psi.search.SearchScope;
import org.ax1.lisp.usages.LispFunctionReference;
import org.ax1.lisp.usages.LispVariableReference;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static org.ax1.lisp.psi.impl.LispListMixin.ListSyntaxType.*;

public abstract class LispSymbolMixinImpl extends ASTWrapperPsiElement implements PsiNameIdentifierOwner, LispSymbol {

  private SymbolSyntaxType syntaxType = SymbolSyntaxType.UNKNOWN;

  public LispSymbolMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  public PsiReference getReference() {
    if (isFunctionCall()) {
      return new LispFunctionReference(this);
    }
    if (isVariableReference()) {
      return new LispVariableReference(this);
    }
    return null;
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
  public boolean isFunctionCall() {
    return getSyntaxType() == SymbolSyntaxType.FUNCTION_USAGE;
  }

  @Override
  public boolean isVariableReference() {
    return getSyntaxType() == SymbolSyntaxType.VARIABLE_USAGE;
  }

  @Override
  public boolean isFunctionDefinition() {
    return getSyntaxType() == SymbolSyntaxType.FUNCTION_DEFINITION;
  }

  @Override
  public boolean isVariableDefinition() {
    return isLetVariableName() || isParameterName() || isDestructuringBindVariableName();
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
    for (PsiElement container = getParent().getParent(); container instanceof LispList ; container = container.getParent().getParent()) {
      LispList list = (LispList) container;
      if (list.isFormDestructuringBind()) {
        return contains(list.getDestructuringBindVariableList(), this);
      }
    }
    return false;
  }

  private boolean contains(LispList list, LispSymbol symbol) {
    List<LispSexp> sexpList = list.getSexpList();
    for (LispSexp lispSexp : sexpList) {
      if (lispSexp.getSymbol() == symbol) return true;
      LispList subList = lispSexp.getList();
      if (subList != null && contains(subList, symbol)) return true;
    }
    return false;
  }

  @Override
  public @Nullable PsiElement getNameIdentifier() {
    return this;
  }

  public SymbolSyntaxType getSyntaxType() {
    if (syntaxType == SymbolSyntaxType.UNKNOWN) {
      ((LispFile) getContainingFile()).computeSyntaxType();
      if (syntaxType == SymbolSyntaxType.UNKNOWN) {
        System.err.println("Oh no, syntax type was not updated!");
      }
    }
    return syntaxType;
  }

  public void setSyntaxType(SymbolSyntaxType syntaxType) {
    this.syntaxType = syntaxType;
  }

  @Override
  public LispList getDefunFromParameter() {
    LispList lambdaList = (LispList) getParent().getParent();
    return (LispList) lambdaList.getParent().getParent();
  }

  @Override
  public LispList getVariableContainer() {
    LispList varList = (LispList) getParent().getParent();
    while (varList.getSyntaxType() != FUNCTION_CALL) {
      varList = (LispList) varList.getParent().getParent();
    }
    return varList;
  }

  @Override
  public @NotNull SearchScope getUseScope() {
    if (isVariableDefinition()) {
      return new LocalSearchScope(getVariableContainer());
    }
    return super.getUseScope();
  }
}