package org.ax1.lisp.analysis.symbol;

public class KeywordPackage extends LispPackage {

  public KeywordPackage() {
    super("KEYWORD");
    setReadOnly();
  }

  @Override
  public Symbol intern(String symbolName) {
    Symbol symbol = super.intern(symbolName);
    getVariable(symbol).setKeyword(true);
    return symbol;
  }
}
