package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiQualifiedNamedElement;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.analysis.symbol.PackageDefinition;

public interface LispSexpMixin extends PsiNameIdentifierOwner, PsiQualifiedNamedElement {

  boolean isFunctionCall();
  boolean isVariableReference();
  boolean isFunctionDefinition();
  boolean isVariableDefinition();
  boolean isLexicalDefinition();
  PackageDefinition getPackageDefinition();
  SymbolBinding getSymbolDefinition();
  LispDefinition getDefinition();
}
