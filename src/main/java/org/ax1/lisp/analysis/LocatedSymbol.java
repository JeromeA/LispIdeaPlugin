package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.impl.LispStringDesignator;

public class LocatedSymbol {
  public final Symbol symbol;
  public final LispStringDesignator location;

  public LocatedSymbol(Symbol symbol, LispStringDesignator location) {
    this.symbol = symbol;
    this.location = location;
  }
}
