package org.ax1.lisp.psi.impl;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
import java.util.Objects;

public abstract class LispListMixinImpl extends ASTWrapperPsiElement implements LispList {

  public LispListMixinImpl(@NotNull ASTNode node) {
    super(node);
  }

  public boolean isFormDefun() {
    if (!Objects.equals(getFunctionCallName(), "defun")) return false;
    List<LispSexp> children = getSexpList();
    if (children.size() < 2) return false;
    LispSymbol symbol1 = children.get(1).getSymbol();
    return symbol1 != null;
  }

  public @NotNull List<LispSexp> getDefunLambdaList() {
    return getSexpList().get(2).getList().getSexpList();
  }

  public boolean isFormLet() {
    String functionCallName = getFunctionCallName();
    if (!Objects.equals(functionCallName, "let") && !Objects.equals(functionCallName, "let*")) return false;
    List<LispSexp> children = getSexpList();
    if (children.size() < 2) return false;
    LispList variableList = children.get(1).getList();
    return variableList != null;
  }

  public @NotNull List<LispSexp> getVariableList() {
    return getSexpList().get(1).getList().getSexpList();
  }

  @Nullable
  private String getFunctionCallName() {
    List<LispSexp> children = getSexpList();
    if (children.isEmpty()) return null;
    LispSymbol symbol0 = children.get(0).getSymbol();
    if (symbol0 == null) return null;
    String functionCallName = symbol0.getText();
    return functionCallName;
  }

  public boolean isFunctionCall() {
    LispSexp sexp = (LispSexp) getParent();
    PsiElement parent = sexp.getParent();
    if (parent instanceof LispFile) {
      return true;
    }
    LispList parentList = (LispList) parent;
    return parentList.isFunctionCall();
  }
}
