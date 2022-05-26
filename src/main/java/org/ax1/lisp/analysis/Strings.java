package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import static org.ax1.lisp.psi.LispTypes.STRING;

public class Strings {

  public static String getStringDesignator(SyntaxAnalyzer analyzer, LispSexp nameDesignator) {
    if (nameDesignator.getList() != null) return null;
    LispSymbol symbolName = nameDesignator.getSymbol();
    if (symbolName != null) {
      analyzer.annotations.highlightConstant(symbolName);
      Symbol symbol = analyzer.packageManager.getSymbol(symbolName);
      return symbol.getName();
    }
    ASTNode token = nameDesignator.getFirstChild().getNode();
    if (token.getElementType() == STRING) {
      String text = token.getText();
      return text.substring(1, text.length() - 1);
    }
    return null;
  }

  public static String getString(LispSexp sexp) {
    // TODO: do the real thing.
    return sexp.getText();
  }
}
