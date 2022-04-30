package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

public class SymbolBinding {
  private final String name;
  private String description;

  private LispList container;
  private LispSymbol definition;
  private final Set<LispSymbol> usages = new HashSet<>();

  private final SymbolType symbolType;
  private final BindingType bindingType;

  public SymbolBinding(String name, SymbolType symbolType, BindingType bindingType) {
    this.name = name;
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

  public boolean isDefined() {
    return definition != null || description != null;
  }

  public SymbolType getSymbolType() {
    return symbolType;
  }

  public BindingType getBindingType() {
    return bindingType;
  }

  public String getName() {
    return name;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public static SymbolBinding merge(Collection<SymbolBinding> bindings) {
    SymbolBinding firstBinding = getFirstElement(bindings);
    if (bindings.size() == 1) return firstBinding;
    SymbolBinding result = new SymbolBinding(firstBinding.getName(), firstBinding.getSymbolType(), firstBinding.getBindingType());
    bindings.forEach(binding -> result.add(binding));
    return result;
  }

  private void add(SymbolBinding binding) {
    if (binding.description != null) description = binding.description;
    if (binding.container != null) container = binding.container;
    if (binding.definition != null) {
      definition = binding.definition;
      binding.definition.setSymbolBinding(this);
    }
    binding.usages.forEach(usage -> {
      usages.add(usage);
      usage.setSymbolBinding(this);
    });
  }

  private static <T> T getFirstElement(Collection<T> collection) {
    return collection.iterator().next();
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
