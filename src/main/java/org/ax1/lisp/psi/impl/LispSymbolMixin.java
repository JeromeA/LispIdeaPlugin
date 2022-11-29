package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiNameIdentifierOwner;
import org.ax1.lisp.analysis.SymbolBinding;
import org.ax1.lisp.psi.LispList;

public interface LispSymbolMixin extends PsiNameIdentifierOwner {

  boolean isFunctionCall();

  boolean isVariableReference();

  boolean isFunctionDefinition();

  boolean isVariableDefinition();

  boolean isLetVariableName();

  boolean isLexicalDefinition();

  boolean isParameterName();

  boolean isDestructuringBindVariableName();

  LispList getDefunFromParameter();

  SymbolBinding getSymbolDefinition();
}
