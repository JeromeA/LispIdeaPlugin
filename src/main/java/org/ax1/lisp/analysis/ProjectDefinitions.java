package org.ax1.lisp.analysis;

import com.google.common.collect.ImmutableList;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static org.ax1.lisp.analysis.SymbolBinding.Type.VARIABLE;

public class ProjectDefinitions {

  private final Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private final Map<Symbol, SymbolBinding> variables = new HashMap<>();
  private final Map<LispSymbol, SymbolBinding> symbolDefinitions = new HashMap<>();
  private final Map<LispSexp, PackageDefinition> packageDefinitions = new HashMap<>();

  public ProjectDefinitions(ImmutableList<Bindings> results) {
    for (Bindings result : results) {
      for (SymbolBinding definition : result.definitions) {
        add(definition.type == VARIABLE ? variables : functions, definition);
      }
    }
    for (SymbolBinding definition : functions.values()) {
      definition.definitions.forEach(def -> symbolDefinitions.put(def, definition));
      definition.methods.forEach(def -> symbolDefinitions.put(def, definition));
      definition.usages.forEach(def -> symbolDefinitions.put(def, definition));
    }
    for (SymbolBinding definition : variables.values()) {
      definition.definitions.forEach(def -> symbolDefinitions.put(def, definition));
      definition.usages.forEach(def -> symbolDefinitions.put(def, definition));
    }
  }

  private void add(Map<Symbol, SymbolBinding> definitionMap, SymbolBinding definition) {
    definitionMap.merge(definition.symbol, definition, ProjectDefinitions::mergeDefinitions);
  }

  private static SymbolBinding mergeDefinitions(SymbolBinding def1, SymbolBinding def2) {
    def1.definitions.addAll(def2.definitions);
    def1.methods.addAll(def2.methods);
    def1.usages.addAll(def2.usages);
    if (def2.description != null) def1.description = def2.description;
    return def1;
  }

  public Collection<SymbolBinding> getFunctions() {
    return functions.values();
  }

  public Collection<SymbolBinding> getVariables() {
    return variables.values();
  }

  public SymbolBinding getDefinition(LispSymbol lispSymbol) {
    return symbolDefinitions.get(lispSymbol);
  }

  public PackageDefinition getPackage(LispSexp lispSexp) {
    return packageDefinitions.get(lispSexp);
  }
}
