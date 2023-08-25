package org.ax1.lisp.analysis.symbol;

import com.intellij.psi.PsiElement;
import org.ax1.lisp.SymbolResolver;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbolName;

public class LexicalVariables {

  public static LexicalSymbol find(LispSymbolName symbolName) {
    Symbol symbol = SymbolResolver.resolve(symbolName);
    LispSexp parent = getParent(symbolName);
    while (parent != null) {
      LexicalSymbol variable = parent.getLexicalVariables().get(symbol);
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
