package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface LispListMixin extends PsiElement {

  boolean isFormDefun();
  boolean isFormLet();

  @NotNull List<LispSexp> getDefunLambdaList();
  @NotNull List<LispSexp> getVariableList();

  boolean isFunctionCall();
}
