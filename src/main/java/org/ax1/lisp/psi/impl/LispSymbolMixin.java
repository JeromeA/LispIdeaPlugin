package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiNameIdentifierOwner;
import org.ax1.lisp.psi.LispList;

public interface LispSymbolMixin extends PsiNameIdentifierOwner {

  boolean isFunctionCall();

  boolean isVariableReference();

  boolean isFunctionDefinition();

  boolean isVariableDefinition();

  boolean isLetVariableName();

  boolean isParameterName();

  boolean isDestructuringBindVariableName();

  SymbolSyntaxType getSyntaxType();

  void setSyntaxType(SymbolSyntaxType syntaxType);

  LispList getDefunFromParameter();

  LispList getVariableContainer();

  enum SymbolSyntaxType {
    UNKNOWN,
    IGNORED, // &KEY, loop keyword, etc.
    DATA, // Inside quote
    FUNCTION_DEFINITION,
    FUNCTION_USAGE,
    VARIABLE_DEFINITION,
    VARIABLE_USAGE,
  }
}
