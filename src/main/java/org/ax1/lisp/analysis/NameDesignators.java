package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

public class NameDesignators {
  public static LispStringDesignator getLispStringDesignator(AnalysisContext context, LispSexp sexp) {
    LispStringDesignator lispPackageName = null;
    if (sexp.getString() != null) {
      lispPackageName = sexp.getString().getStringContent();
    }
    if (sexp.getSymbol() != null) {
      lispPackageName = sexp.getSymbol().getSymbolName();
    }
    if (lispPackageName != null) {
      context.highlighter.highlightDeclaration(lispPackageName);
    }
    return lispPackageName;
  }
}
