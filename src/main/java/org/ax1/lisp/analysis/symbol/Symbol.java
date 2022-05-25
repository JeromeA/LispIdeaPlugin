package org.ax1.lisp.analysis.symbol;

import static org.ax1.lisp.analysis.symbol.CommonLispPackage.COMMON_LISP;
import static org.ax1.lisp.analysis.symbol.KeywordPackage.KEYWORD;

public class Symbol {

  private final String packageName;
  private final String name;

  public Symbol(String packageName, String name) {
    this.packageName = packageName;
    this.name = name;
  }

  public static Symbol clSymbol(String name) {
    return new Symbol(COMMON_LISP, name);
  }

  public static Symbol keywordSymbol(String name) {
    return new Symbol(KEYWORD, name);
  }

  public String getName() {
    return name;
  }

  public String getPackageName() {
    return packageName;
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
