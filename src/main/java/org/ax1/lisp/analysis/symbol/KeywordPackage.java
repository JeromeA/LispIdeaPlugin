package org.ax1.lisp.analysis.symbol;

public class KeywordPackage extends LispPackage {

  public KeywordPackage() {
    super("KEYWORD");
    setStandardPackage(true);
  }
}
