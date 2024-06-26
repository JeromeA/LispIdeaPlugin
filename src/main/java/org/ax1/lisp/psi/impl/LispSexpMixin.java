package org.ax1.lisp.psi.impl;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.analysis.BaseLispElement;
import org.ax1.lisp.analysis.symbol.LexicalSymbol;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSymbolName;

import java.util.Collection;
import java.util.Map;

public interface LispSexpMixin extends PsiElement, BaseLispElement {
  boolean isSymbol();
  LispStringDesignator getStringDesignator();
  LispSymbolName getSymbolName();

  void addLexicalVariables(Collection<LexicalSymbol> variables);
  void addLexicalFunctions(Collection<LexicalSymbol> functions);

  Map<Symbol, LexicalSymbol> getLexicalVariables();
  Map<Symbol, LexicalSymbol> getLexicalFunctions();

  String getPackageContext();
  // Each sexp can have a package context (and not only symbols), so that we know what package to use when
  // running an eval on this sexp.
  void setPackageContext(String packageContext);
}
