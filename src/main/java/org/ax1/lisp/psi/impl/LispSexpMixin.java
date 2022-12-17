package org.ax1.lisp.psi.impl;

import com.intellij.psi.ExternallyAnnotated;
import com.intellij.psi.PsiNameIdentifierOwner;
import com.intellij.psi.PsiQualifiedNamedElement;
import org.ax1.lisp.analysis.symbol.LispDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.psi.LispSexp;

public interface LispSexpMixin extends PsiNameIdentifierOwner, PsiQualifiedNamedElement, ExternallyAnnotated {

  boolean isFunctionDefinition();
  boolean isVariableDefinition();
  PackageDefinition getPackageDefinition();
  SymbolDefinition getSymbolDefinition();
  LispDefinition getDefinition();
  boolean isString();
}
