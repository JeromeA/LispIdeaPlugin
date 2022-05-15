package org.ax1.lisp.analysis;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

public class ProjectSymbolAnalysis {
  public Map<LispSymbol, SymbolBinding> bindings = new HashMap<>();
  public Map<Symbol, SymbolBinding> functions = new HashMap<>();
  public Map<Symbol, SymbolBinding> variables = new HashMap<>();

  private ProjectSymbolAnalysis() {}

  public static Builder newBuilder() {
    return new Builder();
  }

  private void addBinding(SymbolBinding binding) {
    if (binding.getDefinition() != null) bindings.put(binding.getDefinition(), binding);
    binding.getUsages().forEach(u -> bindings.put(u, binding));
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

    private Builder() {}

    public void addFileAnalysis(FileSymbolAnalysis fileSymbolAnalysis) {
      bindings.addAll(fileSymbolAnalysis.retiredBindings);
      fileSymbolAnalysis.functions.forEach(functions::put);
      fileSymbolAnalysis.variables.forEach(variables::put);
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
      return analysis;
    }

  }
}
