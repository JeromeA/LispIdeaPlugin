package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSymbol;

public class LocatedSymbol {
  public final Symbol symbol;
  public final LispSymbol location;

  public LocatedSymbol(Symbol symbol, LispSymbol location) {
    this.symbol = symbol;
    this.location = location;
  }
}
