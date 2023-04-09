package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.analysis.symbol.SymbolDefinition.Type;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.symbol.SymbolDefinition.Scope.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolDefinition.newDefinition;

public class LexicalBindingManager {

  private final Stack<Map<Symbol, SymbolDefinition>> functions = new Stack<>();
  private final Stack<Map<Symbol, SymbolDefinition>> variables = new Stack<>();
  private final List<SymbolDefinition> retiredBindings = new ArrayList<>();

  public void dropLexicalVariables() {
    retiredBindings.addAll(variables.pop().values());
  }

  public void dropLexicalFunctions() {
    retiredBindings.addAll(functions.pop().values());
  }

  @NotNull
  public LexicalScope defineLexicalVariables(Collection<SymbolDefinition> bindings) {
    Map<Symbol, SymbolDefinition> newDictionary = new HashMap<>();
    for (SymbolDefinition binding : bindings) {
      newDictionary.put(binding.symbol, binding);
    }
    variables.push(newDictionary);
    return this::dropLexicalVariables;
  }

  public LexicalScope defineLexicalFunctions(List<LocatedSymbol> functionList) {
    Map<Symbol, SymbolDefinition> newDictionary = new HashMap<>();
    for (LocatedSymbol function : functionList) {
      newDictionary.put(function.symbol, newDefinition(Type.FUNCTION, LEXICAL, function));
    }
    functions.push(newDictionary);
    return this::dropLexicalFunctions;
  }

  public LexicalScope defineLexicalFunction(Symbol symbol) {
    Map<Symbol, SymbolDefinition> newDictionary = new HashMap<>();
    newDictionary.put(symbol, newDefinition(Type.FUNCTION, LEXICAL, symbol));
    functions.push(newDictionary);
    return this::dropLexicalFunctions;
  }

  public SymbolDefinition getLexicalVariable(Symbol symbol) {
    return getSymbolDefinition(variables, symbol);
  }

  public SymbolDefinition getLexicalFunction(Symbol symbol) {
    return getSymbolDefinition(functions, symbol);
  }

  @Nullable
  private SymbolDefinition getSymbolDefinition(Stack<Map<Symbol, SymbolDefinition>> functions, Symbol symbol) {
    for (int i = functions.size() - 1; i >= 0; i--) {
      SymbolDefinition SymbolDefinition = functions.get(i).get(symbol);
      if (SymbolDefinition != null) return SymbolDefinition;
    }
    return null;
  }

  public boolean isEmpty() {
    return functions.empty() && variables.empty();
  }

  public Collection<SymbolDefinition> getRetiredBindings() {
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
