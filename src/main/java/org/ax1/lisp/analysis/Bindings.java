package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

import static org.ax1.lisp.analysis.SymbolBinding.*;

public class Bindings {
  public final Collection<PackageDefinition> packages = new ArrayList<>();
  public final Collection<SymbolBinding> definitions = new ArrayList<>();
  private final Collection<SymbolBinding> conditionDefinitions = new ArrayList<>();

  public void addFunctionDefinition(Symbol functionName, LispSymbol location, String description) {
    definitions.add(newDefinition(Type.FUNCTION, Scope.DYNAMIC, functionName, location, description));
  }

  public void addFunctionDefinition(Symbol functionName, String description) {
    definitions.add(newDefinition(Type.FUNCTION, Scope.DYNAMIC, functionName, description));
  }

  public void addMethodDefinition(Symbol methodName, LispSymbol location, String description) {
    definitions.add(newMethod(methodName, location, description));
  }

  public void addDefPackage(PackageDefinition definition) {
    packages.add(definition);
  }

  public void addInPackage(String name, LispSexp sexp) {
    PackageDefinition packageDefinition = new PackageDefinition(name);
    packageDefinition.usages.add(sexp);
    packages.add(packageDefinition);
  }

  public void addVariableDefinition(Symbol variableName, LispSymbol location, String description) {
    definitions.add(newDefinition(Type.VARIABLE, Scope.DYNAMIC, variableName, location, description));
  }

  public void addFunctionUsage(Symbol functionName, LispSymbol location) {
    definitions.add(newUsage(Type.FUNCTION, Scope.DYNAMIC, functionName, location));
  }

  public void addVariableUsage(Symbol variableName, LispSymbol location) {
    definitions.add(newUsage(Type.VARIABLE, Scope.DYNAMIC, variableName, location));
  }

  public void addLexicalBindings(Collection<SymbolBinding> nindings) {
    definitions.addAll(nindings);
  }

  public void addPackages(List<PackageDefinition> packagesToAdd) {
    packages.addAll(packagesToAdd);
  }
}
