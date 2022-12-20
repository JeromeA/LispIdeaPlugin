package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.*;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.*;

public class Bindings {
  public final Collection<PackageDefinition> packages = new ArrayList<>();
  public final Collection<SymbolDefinition> definitions = new ArrayList<>();
  private final Collection<SymbolDefinition> conditionDefinitions = new ArrayList<>();

  public void addFunctionDefinition(Symbol functionName, LispStringDesignator location, String description) {
    definitions.add(newDefinition(Type.FUNCTION, Scope.DYNAMIC, functionName, location, description));
  }

  public void addFunctionDefinition(Symbol functionName, String description) {
    definitions.add(newDefinition(Type.FUNCTION, Scope.DYNAMIC, functionName, description));
  }

  public void addMethodDefinition(Symbol methodName, LispStringDesignator location, String description) {
    definitions.add(newMethod(methodName, location, description));
  }

  public void addDefPackage(PackageDefinition definition) {
    packages.add(definition);
  }

  public void addPackageUsage(LispStringDesignator packageName) {
    PackageDefinition packageDefinition = new PackageDefinition(packageName.getValue());
    packageDefinition.getUsages().add(packageName);
    packages.add(packageDefinition);
  }

  public void addVariableDefinition(Symbol variableName, LispStringDesignator location, String description) {
    definitions.add(newDefinition(Type.VARIABLE, Scope.DYNAMIC, variableName, location, description));
  }

  public void addVariableDefinition(Symbol variableName, String description) {
    definitions.add(newDefinition(Type.VARIABLE, Scope.DYNAMIC, variableName, description));
  }

  public void addFunctionUsage(Symbol functionName, LispStringDesignator location) {
    definitions.add(newUsage(Type.FUNCTION, Scope.DYNAMIC, functionName, location));
  }

  public void addVariableUsage(Symbol variableName, LispStringDesignator location) {
    definitions.add(newUsage(Type.VARIABLE, Scope.DYNAMIC, variableName, location));
  }

  public void addLexicalBindings(Collection<SymbolDefinition> nindings) {
    definitions.addAll(nindings);
  }

  public void addPackages(List<PackageDefinition> packagesToAdd) {
    packages.addAll(packagesToAdd);
  }
}
