package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.impl.LispStringDesignator;
import org.jetbrains.annotations.NotNull;

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
      symbol = createSymbol(symbolName);
    }
    return symbol;
  }

  @NotNull
  private Symbol createSymbol(String symbolName) {
    Symbol symbol;
    symbol = new Symbol(definition.name, symbolName);
    symbols.put(symbolName, symbol);
    return symbol;
  }

  public Symbol findSymbol(String symbolName) {
    // Local (or cached) symbol.
    Symbol localSymbol = symbols.get(symbolName);
    if (localSymbol != null) return localSymbol;

    // IMPORT-FROM option.
    String importFromPackage = definition.importFrom.get(symbolName);
    if (importFromPackage != null) {
      LispPackage lispPackage = packageManager.getPackage(importFromPackage);
      if (lispPackage != null) {
        // This may be null, but anyway, this symbol is declared as imported, we can't proceed with other options.
        Symbol importedSymbol = lispPackage.findExportedSymbol(symbolName);
        if (importedSymbol != null) {
          symbols.put(symbolName, importedSymbol);
        }
        return importedSymbol;
      }
    }

    // USE option.
    for (String packageName : definition.use.keySet()) {
      LispPackage lispPackage = packageManager.getPackage(packageName);
      if (lispPackage != null) {
        Symbol importedSymbol = lispPackage.findExportedSymbol(symbolName);
        if (importedSymbol != null) {
          symbols.put(symbolName, importedSymbol);
          return importedSymbol;
        }
      }
    }

    // If it's exported, we behave as if it was always there.
    if (definition.exports.containsKey(symbolName)) {
      return createSymbol(symbolName);
    }

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
