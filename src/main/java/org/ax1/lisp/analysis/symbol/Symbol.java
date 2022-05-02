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

  @Override
  public int hashCode() {
    return packageName.hashCode() ^ name.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof Symbol)) return false;
    Symbol that = (Symbol) obj;
    return this.packageName.equals(that.packageName) && this.name.equals(that.name);
  }

  @Override
  public String toString() {
    return packageName + ":" + name;
  }
}
