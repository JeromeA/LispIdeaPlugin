package org.ax1.lisp.analysis.symbol;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Symbol {

  private static final Map<Symbol, Symbol> interned = new HashMap<>();
  private final String packageName;
  private final String name;

  public Symbol(String packageName, String name) {
    this.packageName = packageName;
    this.name = name;
  }

  public static Symbol clSymbol(String name) {
    return new Symbol(CommonLispPackage.COMMON_LISP, name);
  }

  public static Symbol keywordSymbol(String name) {
    return new Symbol("", name);
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
    return getQualifiedName();
  }

  public boolean isConstant() {
    return packageName.equals("")
        || Objects.equals(this, CommonLispPackage.INSTANCE.NIL)
        || Objects.equals(this, CommonLispPackage.INSTANCE.T);
  }

  public String getQualifiedName() {
    return packageName + ":" + name;
  }

  public Symbol intern() {
    Symbol internedSymbol = interned.get(this);
    if (internedSymbol != null) return internedSymbol;
    interned.put(this, this);
    return this;
  }
}
