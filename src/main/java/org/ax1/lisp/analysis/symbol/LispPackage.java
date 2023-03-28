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
    // Local symbol.
    Symbol localSymbol = symbols.get(symbolName);
    if (localSymbol != null) return localSymbol;

    // USE option.
    for (String packageName : definition.use.keySet()) {
      LispPackage lispPackage = packageManager.getPackage(packageName);
      if (lispPackage != null) {
        Symbol usedSymbol = lispPackage.findExportedSymbol(symbolName);
        if (usedSymbol != null) {
          return usedSymbol;
        }
      }
    }

    // IMPORT-FROM option.
    String importFromPackage = definition.importFrom.get(symbolName);
    if (importFromPackage != null) {
      LispPackage lispPackage = packageManager.getPackage(importFromPackage);
      if (lispPackage != null) {
        Symbol symbol = lispPackage.findExportedSymbol(symbolName);
        if (symbol != null) {
          return symbol;
        }
      }
    }

    // EXPORT option.
    Symbol exportedSymbol = findExportedSymbol(symbolName);
    if (exportedSymbol != null) return exportedSymbol;
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
