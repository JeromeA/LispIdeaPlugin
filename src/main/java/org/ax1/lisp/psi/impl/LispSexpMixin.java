package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.psi.LispSymbolName;

public interface LispSexpMixin extends PsiElement {
  boolean isSymbol();
  LispStringDesignator getStringDesignator();
  LispSymbolName getSymbolName();
}
