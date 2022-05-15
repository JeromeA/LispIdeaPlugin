package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class SymbolBinding {
  private final Symbol symbol;
  private String description;

  /**
   * Structure defining the binding. This is for documentation purpose, as in: "Slot 'volume' is declared
   * in 'defstruct bottle'".
   */
  private LispList container;

  private LispSymbol definition;
  private final Set<LispSymbol> usages = new HashSet<>();

  private final SymbolType symbolType;
  private final BindingType bindingType;
  private boolean isKeyword;

  public SymbolBinding(Symbol symbol, SymbolType symbolType, BindingType bindingType) {
    this.symbol = symbol;
    this.symbolType = symbolType;
    this.bindingType = bindingType;
  }

  public Symbol getSymbol() {
    return symbol;
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

  public boolean isDefined() {
    return definition != null || description != null;
  }

  public SymbolType getSymbolType() {
    return symbolType;
  }

  public BindingType getBindingType() {
    return bindingType;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public boolean isKeyword() {
    return isKeyword;
  }

  public void setKeyword(boolean isKeyword) {
    this.isKeyword = isKeyword;
  }

  public void add(SymbolBinding binding) {
    if (binding.description != null) description = binding.description;
    if (binding.container != null) container = binding.container;
    if (binding.definition != null) {
      definition = binding.definition;
    }
    binding.usages.forEach(usage -> {
      usages.add(usage);
    });
  }

  public static SymbolBinding merge(Collection<SymbolBinding> symbolBindings) {
    SymbolBinding first = symbolBindings.iterator().next();
    SymbolBinding symbolBinding = new SymbolBinding(first.symbol, first.symbolType, first.bindingType);
    symbolBindings.forEach(symbolBinding::add);
    return symbolBinding;
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
