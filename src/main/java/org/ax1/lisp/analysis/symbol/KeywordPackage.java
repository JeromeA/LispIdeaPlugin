package org.ax1.lisp.analysis.symbol;

public class KeywordPackage extends LispPackage {

  public static KeywordPackage INSTANCE = new KeywordPackage();

  private KeywordPackage() {
    super(createDefinition());
  }

  private static PackageDefinition createDefinition() {
    PackageDefinition definition = new PackageDefinition("KEYWORD");
    definition.setReadOnly();
    return definition;
  }

  @Override
  public Symbol intern(String symbolName) {
    Symbol symbol = super.intern(symbolName);
    getVariable(symbol).setKeyword(true);
    return symbol;
  }
}
