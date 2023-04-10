package org.ax1.lisp.analysis.symbol;

import java.util.Objects;

import static org.ax1.lisp.analysis.symbol.CommonLispPackage.*;
import static org.ax1.lisp.analysis.symbol.KeywordPackage.KEYWORD;

public class Symbol {

  public static final Symbol NIL = CommonLispPackage.INSTANCE.NIL;
  public static final Symbol T = CommonLispPackage.INSTANCE.T;
  private final String packageName;
  private final String name;

  public Symbol(String packageName, String name) {
    this.packageName = packageName;
    this.name = name;
  }

  public static Symbol clSymbol(String name) {
    return CommonLispPackage.INSTANCE.intern(name);
  }

  public static Symbol keywordSymbol(String name) {
    return KeywordPackage.INSTANCE.intern(name);
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
    return packageName.equals(KEYWORD) || Objects.equals(this, NIL) || Objects.equals(this, T);
  }

  public String getQualifiedName() {
    return packageName + ":" + name;
  }
}
