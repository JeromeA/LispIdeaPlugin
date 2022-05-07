package org.ax1.lisp.analysis.symbol;

import org.jetbrains.annotations.NotNull;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.DYNAMIC;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

public class LispPackage {
  private final String name;
  private Set<String> nicknames = Set.of();
  private final Set<String> use = new HashSet<>();
  protected final Map<String, Symbol> symbols = new HashMap<>();
  private final Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private final Map<Symbol, SymbolBinding> variables = new HashMap<>();
  private boolean isWriteable = true;

  public LispPackage(String name) {
    this.name = name;
  }

  public void setNicknames(Set<String> nicknames) {
    this.nicknames = nicknames;
  }

  public Set<String> getNicknames() {
    return nicknames;
  }

  public String getName() {
    return name;
  }

  public void addUse(String packageName) {
    use.add(packageName);
  }

  public Symbol intern(SymbolManager symbolManager, String symbolName) {
    Symbol symbol = symbols.get(symbolName);
    if (symbol == null) {
      for (String packageName : use) {
        LispPackage lispPackage = symbolManager.getPackage(packageName);
        if (lispPackage.isExporting(symbolName)) {
          return lispPackage.intern(symbolManager, symbolName);
        }
      }
      symbol = intern(symbolName);
    }
    return symbol;
  }

  public Symbol intern(String symbolName) {
    Symbol symbol = symbols.get(symbolName);
    if (symbol == null ) {
      symbol = new Symbol(name, symbolName);
      symbols.put(symbolName, symbol);
    }
    return symbol;
  }

  private boolean isExporting(String symbolName) {
    // TODO: do the right thing.
    return symbols.containsKey(symbolName);
  }

  public Collection<Symbol> getSymbols() {
    return symbols.values();
  }

  public boolean isWriteable() {
    return isWriteable;
  }

  public void setReadOnly() {
    isWriteable = false;
  }

  public Collection<SymbolBinding> getFunctions() {
    return functions.values();
  }

  public Collection<SymbolBinding> getVariables() {
    return variables.values();
  }

  @NotNull
  private SymbolBinding getBinding(Map<Symbol, SymbolBinding> map, Symbol symbol, SymbolBinding.SymbolType symbolType) {
    SymbolBinding binding = map.get(symbol);
    if (binding == null) {
      binding = new SymbolBinding(symbol, symbolType, DYNAMIC);
      map.put(symbol, binding);
    }
    return binding;
  }

  public SymbolBinding getFunction(Symbol symbol) {
    return getBinding(functions, symbol, FUNCTION);
  }


  public SymbolBinding getVariable(Symbol symbol) {
    return getBinding(variables, symbol, VARIABLE);
  }

  public void add(LispPackage packageToAdd) {
    symbols.putAll(packageToAdd.symbols);
    addBindings(functions, packageToAdd.functions.values());
    addBindings(variables, packageToAdd.variables.values());
  }

  private static void addBindings(Map<Symbol, SymbolBinding> map, Collection<SymbolBinding> bindings) {
    bindings.forEach(symbolBinding -> addBinding(map, symbolBinding));
  }

  private static void addBinding(Map<Symbol, SymbolBinding> map, SymbolBinding binding) {
    if (map.containsKey(binding.getSymbol())) {
      map.get(binding.getSymbol()).add(binding);
    } else {
      map.put(binding.getSymbol(), binding);
    }
  }

  public Collection<SymbolBinding> getBindings() {
    return Stream.concat(functions.values().stream(), variables.values().stream()).collect(Collectors.toList());
  }
}
