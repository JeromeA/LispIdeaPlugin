package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolManager;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import static org.ax1.lisp.psi.LispTypes.STRING;

public class StringDesignator {

  public static String getStringDesignator(LispSexp nameDesignator, Annotate annotate, SymbolManager symbolManager) {
    if (nameDesignator.getList() != null) return null;
    LispSymbol symbolName = nameDesignator.getSymbol();
    if (symbolName != null) {
      annotate.highlightConstant(symbolName);
      Symbol symbol = symbolManager.getSymbol(symbolName.getText());
      return symbol.getName();
    }
    ASTNode token = nameDesignator.getFirstChild().getNode();
    if (token.getElementType() == STRING) {
      String text = token.getText();
      return text.substring(1, text.length() - 1);
    }
    return null;
  }

}