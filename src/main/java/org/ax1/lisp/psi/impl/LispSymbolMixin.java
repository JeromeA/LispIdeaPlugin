package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiQualifiedNamedElement;
import org.ax1.lisp.analysis.SymbolBinding;

public interface LispSymbolMixin extends PsiQualifiedNamedElement, PsiNameIdentifierOwner {

  boolean isFunctionCall();

  boolean isVariableReference();

  boolean isFunctionDefinition();

  boolean isVariableDefinition();

  boolean isLexicalDefinition();

  SymbolBinding getSymbolDefinition();
}
