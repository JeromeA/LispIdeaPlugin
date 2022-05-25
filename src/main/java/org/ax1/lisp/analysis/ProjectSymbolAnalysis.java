package org.ax1.lisp.analysis;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import org.ax1.lisp.analysis.symbol.PackageDefinition;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

public class ProjectSymbolAnalysis {
  /**
   * For each LispSymbol (variable, function, package), we need to find:
   * - if it has a declaration (highlighting)
   * - if it has any usages (highlighting)
   * - its LispSymbol declaration (navigation)
   * - its description (tooltip documentation)
   * All these are found in a SymbolBinding.
   */
  public Map<LispSymbol, SymbolBinding> bindings = new HashMap<>();

  public Map<LispSymbol, PackageDefinition> packages = new HashMap<>();

  /**
   * For completion purposes, we need to know all the global functions and variables.
   * TODO: use the SymbolBinding to annotation completions.
   */
  public Map<Symbol, SymbolBinding> functions = new HashMap<>();
  public Map<Symbol, SymbolBinding> variables = new HashMap<>();

  private ProjectSymbolAnalysis() {}

  public static Builder newBuilder() {
    return new Builder();
  }

  private void addBinding(SymbolBinding binding) {
    if (binding.getDefinition() != null) bindings.put(binding.getDefinition(), binding);
    binding.getUsages().forEach(u -> bindings.put(u, binding));
    binding.getMethods().forEach(m -> bindings.put(m, binding));
  }

  private void addPackage(PackageDefinition packageDefinition) {
    if (packageDefinition.getDefinition() != null) packages.put(packageDefinition.getDefinition(), packageDefinition);
    packageDefinition.getUsages().forEach(u -> packages.put(u, packageDefinition));
  }

  private void addFunction(SymbolBinding binding) {
    functions.put(binding.getSymbol(), binding);
    addBinding(binding);
  }

  private void addVariable(SymbolBinding binding) {
    variables.put(binding.getSymbol(), binding);
    addBinding(binding);
  }

  public static class Builder {
    private final List<SymbolBinding> bindings = new ArrayList<>();
    private final Multimap<Symbol, SymbolBinding> functions = ArrayListMultimap.create();
    private final Multimap<Symbol, SymbolBinding> variables = ArrayListMultimap.create();
    private final Set<PackageDefinition> packages = new HashSet<>();

    private Builder() {}

    public void addFileAnalysis(FileSymbolAnalysis fileSymbolAnalysis) {
      bindings.addAll(fileSymbolAnalysis.retiredBindings);
      fileSymbolAnalysis.functions.forEach(functions::put);
      fileSymbolAnalysis.variables.forEach(variables::put);
    }

    public void addPackage(PackageDefinition packageDefinition) {
      packages.add(packageDefinition);
    }

    public ProjectSymbolAnalysis build() {
      ProjectSymbolAnalysis analysis = new ProjectSymbolAnalysis();
      bindings.forEach(analysis::addBinding);
      functions.asMap().values().stream()
          .map(SymbolBinding::merge)
          .forEach(analysis::addFunction);
      variables.asMap().values().stream()
          .map(SymbolBinding::merge)
          .forEach(analysis::addVariable);
      packages.forEach(analysis::addPackage);
      return analysis;
    }

  }
}
