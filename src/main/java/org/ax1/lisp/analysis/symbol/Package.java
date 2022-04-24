package org.ax1.lisp.analysis.symbol;

import java.util.*;

public class Package {
  private final String name;
  private Set<String> nicknames = Set.of();
  private final Set<Package> use = new HashSet<>();
  protected final Map<String, Symbol> symbols = new HashMap<>();

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

  public void addUse(Package aPackage) {
    use.add(aPackage);
  }

  public Symbol intern(String symbolName) {
    Symbol symbol = symbols.get(symbolName);
    if (symbol == null) {
      for (Package aPackage : use) {
        if (aPackage.isExporting(symbolName)) {
          return aPackage.intern(symbolName);
        }
      }
      symbol = new Symbol(symbolName);
      symbols.put(symbolName, symbol);
    }
    return symbol;
  }

  private boolean isExporting(String symbolName) {
    // TODO: do the right thing.
    return symbols.containsKey(symbolName);
  }
}
