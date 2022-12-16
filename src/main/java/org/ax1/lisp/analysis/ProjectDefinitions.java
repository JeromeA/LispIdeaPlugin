package org.ax1.lisp.analysis;

import com.google.common.collect.ImmutableList;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispSexp;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.Type.VARIABLE;

public class ProjectDefinitions {

  private final Map<Symbol, SymbolBinding> functions = new HashMap<>();
  private final Map<Symbol, SymbolBinding> variables = new HashMap<>();
  private final Map<String, PackageDefinition> packages = new HashMap<>();
  private final Map<String, String> packageNames = new HashMap<>();
  private final Map<LispSexp, SymbolBinding> definitionByReference = new HashMap<>();
  private final Map<LispSexp, PackageDefinition> packageByReference = new HashMap<>();

  public ProjectDefinitions(ImmutableList<Bindings> results, Collection<PackageDefinition> packageDefinitions) {
    for (PackageDefinition packageDefinition : packageDefinitions) {
      String name = packageDefinition.getName();
      packageNames.put(name, name);
      packageDefinition.getNicknames().forEach(nickname -> packageNames.put(nickname, name));
      packages.put(name, packageDefinition);
    }
    for (Bindings result : results) {
      for (SymbolBinding symbolBinding : result.definitions) {
        if (symbolBinding.scope == LEXICAL) {
          addSymbolReferences(symbolBinding);
        } else {
          merge(symbolBinding.type == VARIABLE ? variables : functions, symbolBinding);
        }
      }
      for (PackageDefinition packageDefinition : result.packages) {
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

  private void addSymbolReferences(SymbolBinding definition) {
    definition.getDefinitions().forEach(def -> definitionByReference.put(def, definition));
    definition.getUsages().forEach(def -> definitionByReference.put(def, definition));
    definition.methods.forEach(def -> definitionByReference.put(def, definition));
  }

  private void merge(Map<Symbol, SymbolBinding> definitionMap, SymbolBinding definition) {
    definitionMap.merge(definition.symbol, definition, ProjectDefinitions::mergeDefinitions);
  }

  private static SymbolBinding mergeDefinitions(SymbolBinding def1, SymbolBinding def2) {
    def1.getDefinitions().addAll(def2.getDefinitions());
    def1.methods.addAll(def2.methods);
    def1.getUsages().addAll(def2.getUsages());
    if (def2.description != null) def1.description = def2.description;
    return def1;
  }

  public Collection<SymbolBinding> getFunctions() {
    return functions.values();
  }

  public Collection<SymbolBinding> getVariables() {
    return variables.values();
  }

  public SymbolBinding getDefinition(LispSexp lispSexp) {
    return definitionByReference.get(lispSexp);
  }

  public PackageDefinition getPackage(LispSexp lispSexp) {
    return packageByReference.get(lispSexp);
  }
}
