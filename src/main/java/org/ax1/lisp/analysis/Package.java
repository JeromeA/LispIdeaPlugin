package org.ax1.lisp.analysis;

import java.util.HashSet;
import java.util.Set;

import static org.ax1.lisp.analysis.SymbolBinding.SymbolType.FUNCTION;

public class Package {
  private final String name;
  private Set<String> nicknames = Set.of();
  private Set<Package> use = new HashSet<>();
  private final Set<String> functions = new HashSet<>();
  private final Set<String> variables = new HashSet<>();

  public Package(String name) {
    this.name = name;
  }

  public void setNicknames(Set<String> nicknames) {
    this.nicknames = nicknames;
  }

  public Set<String> getNicknames() {
    return nicknames;
  }

  public boolean isFunction(String name) {
    return functions.contains(name) || use.stream().anyMatch(p -> p.isFunction(name));
  }

  public boolean isVariable(String name) {
    return variables.contains(name) || use.stream().anyMatch(p -> p.isVariable(name));
  }

  public void addFunction(String functionName) {
    functions.add(functionName);
  }

  public void addVariable(String variableName) {
    variables.add(variableName);
  }

  public boolean isSymbol(SymbolBinding.SymbolType symbolType, String name) {
    return symbolType == FUNCTION ? isFunction(name) : isVariable(name);
  }

  public String getName() {
    return name;
  }

  public void addUse(Package aPackage) {
    use.add(aPackage);
  }
}
