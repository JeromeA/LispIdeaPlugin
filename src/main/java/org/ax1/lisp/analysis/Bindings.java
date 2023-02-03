package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.*;

public class Bindings {
  private final Collection<PackageDefinition> packages = new ArrayList<>();
  private final Collection<SymbolDefinition> definitions = new ArrayList<>();
  private final Collection<SymbolDefinition> conditionDefinitions = new ArrayList<>();

  public Collection<PackageDefinition> getPackages() {
    return packages;
  }

  public Collection<SymbolDefinition> getDefinitions() {
    return definitions;
  }

  public void addDefinition(SymbolDefinition e) {
    definitions.add(e);
  }

  public void addFunctionDefinition(Symbol functionName, LispStringDesignator location) {
    addDefinition(newDefinition(Type.FUNCTION, Scope.DYNAMIC, functionName, location));
  }

  public void addFunctionDefinition(Symbol functionName) {
    addDefinition(newDefinition(Type.FUNCTION, Scope.DYNAMIC, functionName));
  }

  public void addMethodDefinition(Symbol methodName, LispStringDesignator location, String description) {
    addDefinition(newMethod(methodName, location));
  }

  public void addDefPackage(PackageDefinition definition) {
    packages.add(definition);
  }

  public void addPackageUsage(LispStringDesignator packageName) {
    PackageDefinition packageDefinition = new PackageDefinition(packageName.getValue());
    packageDefinition.getUsages().add(packageName);
    packages.add(packageDefinition);
  }

  public void addVariableDefinition(Symbol variableName, LispStringDesignator location) {
    addDefinition(newDefinition(Type.VARIABLE, Scope.DYNAMIC, variableName, location));
  }

  public void addVariableDefinition(Symbol variableName) {
    addDefinition(newDefinition(Type.VARIABLE, Scope.DYNAMIC, variableName));
  }

  public void addFunctionUsage(Symbol functionName, LispStringDesignator location) {
    addDefinition(newUsage(Type.FUNCTION, Scope.DYNAMIC, functionName, location));
  }

  public void addVariableUsage(Symbol variableName, LispStringDesignator location) {
    addDefinition(newUsage(Type.VARIABLE, Scope.DYNAMIC, variableName, location));
  }

  public void addLexicalBindings(Collection<SymbolDefinition> nindings) {
    definitions.addAll(nindings);
  }
}
