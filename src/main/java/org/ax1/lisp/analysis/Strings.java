package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;

import static org.ax1.lisp.psi.LispTypes.STRING;

public class Strings {

  public static String getStringDesignator(AnalysisContext context, LispSexp nameDesignator) {
    if (nameDesignator.getList() != null) return null;
    if (nameDesignator.isSymbol()) {
      context.highlighter.highlightConstant(nameDesignator);
      Symbol symbol = context.packageManager.getSymbol(nameDesignator);
      return symbol.getName();
    }
    return getString(nameDesignator);
  }

  public static String getString(LispSexp sexp) {
    // TODO: do the real thing.
    ASTNode token = sexp.getFirstChild().getNode();
    if (token.getElementType() != STRING) {
      return null;
    }
    String text = token.getText();
    return text.substring(1, text.length() - 1);
  }
}
