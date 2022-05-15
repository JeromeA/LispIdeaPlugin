package org.ax1.lisp.analysis.symbol;

import org.jetbrains.annotations.NotNull;

import java.util.*;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.DYNAMIC;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

public class LispPackage implements Cloneable {
  private final PackageDefinition definition;
  private Map<String, Symbol> symbols = new HashMap<>();
  private Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private Map<Symbol, SymbolBinding> variables = new HashMap<>();

  public LispPackage(PackageDefinition definition) {
    this.definition = definition;
  }

  public Symbol intern(PackageManager packageManager, String symbolName) {
    Symbol symbol = symbols.get(symbolName);
    if (symbol == null) {
      for (String packageName : definition.use) {
        LispPackage lispPackage = packageManager.getPackage(packageName);
        if (lispPackage.isExporting(symbolName)) {
          return lispPackage.intern(packageManager, symbolName);
        }
      }
      symbol = intern(symbolName);
    }
    return symbol;
  }

  public Symbol intern(String symbolName) {
    Symbol symbol = symbols.get(symbolName);
    if (symbol == null ) {
      symbol = new Symbol(definition.name, symbolName);
      symbols.put(symbolName, symbol);
    }
    return symbol;
  }

  private boolean isExporting(String symbolName) {
    // TODO: do the right thing.
    return symbols.containsKey(symbolName);
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

  @Override
  protected LispPackage clone() {
    try {
      LispPackage copy = (LispPackage) super.clone();
      copy.symbols = new HashMap<>(symbols);
      copy.functions = new HashMap<>(functions);
      copy.variables = new HashMap<>(variables);
      return copy;
    } catch (CloneNotSupportedException e) {
      throw new RuntimeException("This can't happen", e);
    }
  }

  public PackageDefinition getDefinition() {
    return definition;
  }

  public String getName() {
    return definition.name;
  }
}
