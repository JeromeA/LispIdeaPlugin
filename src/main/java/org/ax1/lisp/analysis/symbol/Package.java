package org.ax1.lisp.analysis.symbol;

import java.util.*;

public class Package {
  private final String name;
  private Set<String> nicknames = Set.of();
  private final Set<String> use = new HashSet<>();
  protected final Map<String, Symbol> symbols = new HashMap<>();
  private boolean isStandard;

  public Package(String name) {
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
        Package aPackage = symbolManager.getPackage(packageName);
        if (aPackage.isExporting(symbolName)) {
          return aPackage.intern(symbolManager, symbolName);
        }
      }
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

  public boolean isStandardPackage() {
    return isStandard;
  }

  public void setStandardPackage(boolean value) {
    isStandard = value;
  }
}
