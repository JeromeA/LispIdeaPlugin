package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.*;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface LispListMixin extends PsiElement {

  boolean isFormDefun();
  boolean isFormLet();
  boolean isFormDestructuringBind();
  boolean isCreatingBindings();

  @NotNull List<LispSexp> getDefunLambdaList();
  @NotNull List<LispSexp> getVariableList();
  LispList getDestructuringBindVariableList();

  boolean isFunctionCall();

  ListSyntaxType getSyntaxType();

  void setSyntaxType(ListSyntaxType syntaxType);

  LispSymbol getBindingSymbol(String name);

  List<LispSexp> getFormsInScope(LispSymbol variable);

  enum ListSyntaxType {
    UNKNOWN,
    STRUCTURE, // lambda list, varlist in a LET, entry in a COND, etc.
    DATA,
    FUNCTION_CALL,
    VAR_INIT,
  }

}
