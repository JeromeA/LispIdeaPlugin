package org.ax1.lisp.analysis;

import com.google.common.collect.ImmutableList;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.impl.LispStringDesignator;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Type.VARIABLE;

public class ProjectDefinitions {

  private final Map<Symbol, SymbolDefinition> functions = new HashMap<>();
  private final Map<Symbol, SymbolDefinition> variables = new HashMap<>();
  private final Map<String, PackageDefinition> packages = new HashMap<>();
  private final Map<String, String> packageNames = new HashMap<>();
  private final Map<LispStringDesignator, SymbolDefinition> symbolByReference = new HashMap<>();
  private final Map<LispStringDesignator, PackageDefinition> packageByReference = new HashMap<>();

  public ProjectDefinitions(ImmutableList<Bindings> results, Collection<PackageDefinition> packageDefinitions) {
    for (PackageDefinition packageDefinition : packageDefinitions) {
      String name = packageDefinition.getName();
      packageNames.put(name, name);
      packageDefinition.getNicknames().forEach(nickname -> packageNames.put(nickname, name));
      packages.put(name, packageDefinition);
    }
    for (Bindings result : results) {
      for (SymbolDefinition symbolDefinition : result.getDefinitions()) {
        if (symbolDefinition.scope == LEXICAL) {
          addSymbolReferences(symbolDefinition);
        } else {
          merge(symbolDefinition.type == VARIABLE ? variables : functions, symbolDefinition);
        }
      }
      for (PackageDefinition packageDefinition : result.getPackages()) {
        merge(packageDefinition);
      }
    }
    functions.values().forEach(this::addSymbolReferences);
    variables.values().forEach(this::addSymbolReferences);
    packages.values().forEach(this::addPackageReferences);
  }

  private void addPackageReferences(PackageDefinition definition) {
    packageByReference.put(definition.getDefinition(), definition);
    definition.getUsages().forEach(def -> packageByReference.put(def, definition));
  }

  private void merge(PackageDefinition packageDefinition) {
    String packageName = packageNames.get(packageDefinition.getName());
    packages.merge(packageName, packageDefinition, this::mergePackages);
  }

  private PackageDefinition mergePackages(PackageDefinition def1, PackageDefinition def2) {
    def1.getUsages().addAll(def2.getUsages());
    // TODO: add asserts on various contents from def2.
    if (def2.getDefinition() != null && def1.getDefinition() != def2.getDefinition()) {
      System.err.println("Definitions can't be merged");
    }
    return def1;
  }

  private void addSymbolReferences(SymbolDefinition definition) {
    definition.getDefinitions().forEach(def -> symbolByReference.put(def, definition));
    definition.getUsages().forEach(def -> symbolByReference.put(def, definition));
    definition.methods.forEach(def -> symbolByReference.put(def, definition));
  }

  private void merge(Map<Symbol, SymbolDefinition> definitionMap, SymbolDefinition definition) {
    definitionMap.merge(definition.symbol, definition, SymbolDefinition::merge);
  }

  public Collection<SymbolDefinition> getFunctions() {
    return functions.values();
  }

  public Collection<SymbolDefinition> getVariables() {
    return variables.values();
  }

  public SymbolDefinition getSymbolDefinition(LispStringDesignator symbolName) {
    return symbolByReference.get(symbolName);
  }

  public PackageDefinition getPackageDefinition(LispStringDesignator packageName) {
    return packageByReference.get(packageName);
  }

  public SymbolDefinition getFunction(Symbol symbol) {
    return functions.get(symbol);
  }
}
