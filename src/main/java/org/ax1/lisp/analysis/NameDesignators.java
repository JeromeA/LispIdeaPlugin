package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.impl.LispStringDesignator;

public class NameDesignators {
  public static LispStringDesignator getLispStringDesignator(LispSexp sexp) {
    LispStringDesignator stringDesignator = null;
    if (sexp.getString() != null) {
      stringDesignator = sexp.getString().getStringContent();
    }
    if (sexp.getSymbol() != null) {
      stringDesignator = sexp.getSymbol().getSymbolName();
    }
    return stringDesignator;
  }
}
