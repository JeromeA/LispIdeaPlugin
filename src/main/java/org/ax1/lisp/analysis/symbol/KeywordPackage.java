package org.ax1.lisp.analysis.symbol;

public class KeywordPackage extends LispPackage {

  public KeywordPackage() {
    super("KEYWORD");
    setStandardPackage(true);
  }

  @Override
  public Symbol intern(SymbolManager symbolManager, String symbolName) {
    Symbol symbol = super.intern(symbolManager, symbolName);
    symbolManager.getVariable(symbol).setKeyword(true);
    return symbol;
  }
}
