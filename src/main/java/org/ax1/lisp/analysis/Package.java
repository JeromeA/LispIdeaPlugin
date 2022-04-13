package org.ax1.lisp.analysis;

import java.util.HashSet;
import java.util.Set;

import static org.ax1.lisp.analysis.SymbolDescriptor.SymbolType.FUNCTION;

public class Package {
  private final Set<String> names;
  private final Set<Package> uses;
  private final Set<String> functions = new HashSet<>();
  private final Set<String> variables = new HashSet<>();

  public Package(Set<String> names, Set<Package> uses) {
    this.names = names;
    this.uses = uses;
  }

  public Set<String> getNames() {
    return names;
  }

  public boolean isFunction(String name) {
    return functions.contains(name) || uses.stream().anyMatch(p -> p.isFunction(name));
  }

  public boolean isVariable(String name) {
    return variables.contains(name) || uses.stream().anyMatch(p -> p.isVariable(name));
  }

  public void addFunction(String functionName) {
    functions.add(functionName);
  }

  public void addVariable(String variableName) {
    variables.add(variableName);
  }

  public boolean isSymbol(SymbolDescriptor.SymbolType symbolType, String name) {
    return symbolType == FUNCTION ? isFunction(name) : isVariable(name);
  }
}
