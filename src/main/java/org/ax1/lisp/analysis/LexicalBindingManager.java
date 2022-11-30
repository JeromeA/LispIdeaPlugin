package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.SymbolBinding.Type;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.SymbolBinding.Scope.LEXICAL;
import static org.ax1.lisp.analysis.SymbolBinding.newDefinition;

public class LexicalBindingManager {

  private final Stack<Map<Symbol, SymbolBinding>> functions = new Stack<>();
  private final Stack<Map<Symbol, SymbolBinding>> variables = new Stack<>();
  private final List<SymbolBinding> retiredBindings = new ArrayList<>();

  public void dropLexicalVariables() {
    retiredBindings.addAll(variables.pop().values());
  }

  public void dropLexicalFunctions() {
    retiredBindings.addAll(functions.pop().values());
  }

  @NotNull
  public LexicalScope defineLexicalVariables(Collection<SymbolBinding> bindings) {
    Map<Symbol, SymbolBinding> newDictionary = new HashMap<>();
    for (SymbolBinding binding : bindings) {
      newDictionary.put(binding.symbol, binding);
    }
    variables.push(newDictionary);
    return this::dropLexicalVariables;
  }

  public LexicalScope defineLexicalFunctions(List<LocatedSymbol> functionList) {
    Map<Symbol, SymbolBinding> newDictionary = new HashMap<>();
    for (LocatedSymbol function : functionList) {
      newDictionary.put(function.symbol, newDefinition(Type.FUNCTION, LEXICAL, function, ""));
    }
    functions.push(newDictionary);
    return this::dropLexicalFunctions;
  }

  public SymbolBinding getLexicalVariable(Symbol symbol) {
    return getSymbolDefinition(variables, symbol);
  }

  public SymbolBinding getLexicalFunction(Symbol symbol) {
    return getSymbolDefinition(functions, symbol);
  }

  @Nullable
  private SymbolBinding getSymbolDefinition(Stack<Map<Symbol, SymbolBinding>> functions, Symbol symbol) {
    for (int i = functions.size() - 1; i >= 0; i--) {
      SymbolBinding SymbolBinding = functions.get(i).get(symbol);
      if (SymbolBinding != null) return SymbolBinding;
    }
    return null;
  }

  public boolean isEmpty() {
    return functions.empty() && variables.empty();
  }

  public Collection<SymbolBinding> getRetiredBindings() {
    return retiredBindings;
  }

  public List<String> getLexicalVariables() {
    return variables.stream().flatMap(map -> map.keySet().stream())
        .map(Symbol::getName)
        .collect(Collectors.toList());
  }

  public Collection<String> getLexicalFunctions() {
    return functions.stream().flatMap(map -> map.keySet().stream())
        .map(Symbol::getName)
        .collect(Collectors.toList());
  }

  public interface LexicalScope extends AutoCloseable {
    @Override
    void close();
  }

}
