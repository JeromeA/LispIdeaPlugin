package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;

public class LocatedSymbol {
  public final Symbol symbol;
  public final LispSexp location;

  public LocatedSymbol(Symbol symbol, LispSexp location) {
    this.symbol = symbol;
    this.location = location;
  }
}
