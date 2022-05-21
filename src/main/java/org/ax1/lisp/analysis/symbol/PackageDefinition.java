package org.ax1.lisp.analysis.symbol;

import org.ax1.lisp.psi.LispSymbol;

import java.util.HashSet;
import java.util.Set;

public class PackageDefinition {
  final String name;
  private final Set<String> nicknames = new HashSet<>();
  final Set<String> use = new HashSet<>();
  private boolean isWriteable = true;
  private LispSymbol definition;
  private final Set<LispSymbol> usages = new HashSet<>();

  public PackageDefinition(String name) {
    this.name = name;
  }

  public void addNickname(String nickname) {
    nicknames.add(nickname);
  }

  public Set<String> getNicknames() {
    return nicknames;
  }

  public void addUse(String packageName) {
    use.add(packageName);
  }

  public boolean isWriteable() {
    return isWriteable;
  }

  public void setReadOnly() {
    isWriteable = false;
  }

  public void setDefinition(LispSymbol symbol) {
    definition = symbol;
  }

  public void addUsage(LispSymbol symbol) {
    usages.add(symbol);
  }

  public LispSymbol getDefinition() {
    return definition;
  }

  public Set<LispSymbol> getUsages() {
    return usages;
  }
}
