package org.ax1.lisp.analysis.symbol;

import com.intellij.psi.PsiElement;
import kotlin.jvm.functions.Function2;
import org.ax1.lisp.SymbolResolver;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

public class LexicalSymbols {

  public static LexicalSymbol findLexicalVariable(LispSymbolName symbolName) {
    return findLexicalSymbol(symbolName, LexicalSymbols::getLexicalVariable);
  }

  private static LexicalSymbol getLexicalVariable(LispSexp container, Symbol symbol) {
    return container.getLexicalVariables().get(symbol);
  }

  public static LexicalSymbol findLexicalFunction(LispSymbolName symbolName) {
    return findLexicalSymbol(symbolName, LexicalSymbols::getLexicalFunction);
  }

  private static LexicalSymbol getLexicalFunction(LispSexp container, Symbol symbol) {
    return container.getLexicalFunctions().get(symbol);
  }

  private static LexicalSymbol findLexicalSymbol(
      LispSymbolName symbolName, Function2<LispSexp, Symbol, LexicalSymbol> getSymbol) {
    Symbol symbol = SymbolResolver.resolve(symbolName);
    LispSexp parent = getParent(symbolName);
    while (parent != null) {
      LexicalSymbol variable = getSymbol.invoke(parent, symbol);
      if (variable != null) return variable;
      parent = getParent(parent);
    }
    return null;
  }

  private static LispSexp getParent(PsiElement element) {
    PsiElement parent = element.getParent();
    while (parent != null) {
      if (parent instanceof LispSexp) return (LispSexp) parent;
      parent = parent.getParent();
    }
    return null;
  }
}
