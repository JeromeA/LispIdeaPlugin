package org.ax1.lisp;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class SymbolCache {
  private LispList container;
  private final SymbolType symbolType;
  private final BindingType bindingType;
  private LispSymbol definition;
  private final Set<LispSymbol> usages = new HashSet<>();

  public SymbolCache(SymbolType symbolType, BindingType bindingType) {
    this.symbolType = symbolType;
    this.bindingType = bindingType;
  }

  public void addUsage(LispSymbol symbol) {
    usages.add(symbol);
  }

  public void setDefinition(LispList container, LispSymbol symbol) {
    this.container = container;
    definition = symbol;
  }

  public LispList getContainer() {
    return container;
  }

  public Collection<LispSymbol> getUsages() {
    return usages;
  }

  public LispSymbol getDefinition() {
    return definition;
  }

  public SymbolType getSymbolType() {
    return symbolType;
  }

  public BindingType getBindingType() {
    return bindingType;
  }

  public enum SymbolType {
    FUNCTION,
    VARIABLE
  }

  public enum BindingType {
    DYNAMIC,
    LEXICAL
  }
}
