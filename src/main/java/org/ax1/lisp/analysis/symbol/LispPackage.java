package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

public class LispPackage {
  private final PackageManager packageManager;
  private final PackageDefinition definition;
  private Map<String, Symbol> symbols = new HashMap<>();

  public LispPackage(PackageManager packageManager, PackageDefinition definition) {
    this.packageManager = packageManager;
    this.definition = definition;
    definition.exports.keySet().forEach(this::intern);
    definition.shadows.keySet().forEach(this::intern);
  }

  public Symbol intern(String symbolName) {
    Symbol symbol = findSymbol(symbolName);
    if (symbol == null ) {
      symbol = new Symbol(definition.name, symbolName);
      symbols.put(symbolName, symbol);
    }
    return symbol;
  }

  public Symbol findSymbol(String symbolName) {
    Symbol localSymbol = symbols.get(symbolName);
    if (localSymbol != null) return localSymbol;
    for (String packageName : definition.use.keySet()) {
      LispPackage lispPackage = packageManager.getPackage(packageName);
      if (lispPackage != null) {
        Symbol usedSymbol = lispPackage.findExportedSymbol(symbolName);
        if (usedSymbol != null) {
          return usedSymbol;
        }
      }
    }
    Symbol exportedSymbol = findExportedSymbol(symbolName);
    if (exportedSymbol != null) return exportedSymbol;
    // TODO: search importFrom.
    return null;
  }

  public Symbol findExportedSymbol(String symbolName) {
    if (!definition.exports.containsKey(symbolName)) {
      return null;
    }
    return symbols.get(symbolName);
  }

  public PackageDefinition getDefinition() {
    return definition;
  }

  public String getName() {
    return definition.name;
  }
}
