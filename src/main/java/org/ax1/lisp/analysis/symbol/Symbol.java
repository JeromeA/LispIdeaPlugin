package org.ax1.lisp.analysis.symbol;

public class Symbol {

  private String packageName;
  private final String name;

  public Symbol(String packageName, String name) {
    this.packageName = packageName;
    this.name = name;
  }

  public String getName() {
    return name;
  }
}
