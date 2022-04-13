package org.ax1.lisp.analysis;

import com.intellij.lang.ASTNode;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import static org.ax1.lisp.psi.LispTypes.STRING;

public class AnalyzeInPackage implements Analyzer {
  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.highlightKeyword(form);
    if (form.getSexpList().size() != 2) {
      analyzer.highlightError(form, "IN-PACKAGE needs exactly 1 argument");
      return;
    }
    LispSexp arg = form.getSexpList().get(1);
    String stringDesignator = decodeStringDesignator(arg);
    if (stringDesignator == null) {
      analyzer.highlightError(arg, "Expected name designator");
      return;
    }
    if (analyzer.packages.get(stringDesignator) == null) {
      analyzer.highlightUnknown(arg, String.format("Unknown package '%s'", stringDesignator));
      return;
    }
    analyzer.packageName = stringDesignator;
  }

  private static String decodeStringDesignator(LispSexp nameDesignator) {
    if (nameDesignator.getList() != null) return null;
    LispSymbol symbol = nameDesignator.getSymbol();
    if (symbol != null) {
      String text = symbol.getText();
      int colonIndex = text.indexOf(':');
      if (colonIndex >= 0) {
        return text.substring(colonIndex + 1);
      }
      return text;
    }
    ASTNode token = nameDesignator.getFirstChild().getNode();
    if (token.getElementType() == STRING) {
      String text = token.getText();
      return text.substring(1, text.length() - 1);
    }
    return null;
  }
}
