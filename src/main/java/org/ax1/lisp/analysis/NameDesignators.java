package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

public class NameDesignators {
  public static LispStringDesignator getLispStringDesignator(AnalysisContext context, LispSexp sexp) {
    LispStringDesignator stringDesignator = null;
    if (sexp.getString() != null) {
      stringDesignator = sexp.getString().getStringContent();
    }
    if (sexp.getSymbol() != null) {
      stringDesignator = sexp.getSymbol().getSymbolName();
    }
    if (stringDesignator != null) {
      context.highlighter.highlightDeclaration(stringDesignator);
    }
    return stringDesignator;
  }
}
