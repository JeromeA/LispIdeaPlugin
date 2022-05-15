package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;
import java.util.stream.Collectors;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.FUNCTION;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

public class LexicalBindingManager {

  private final Stack<Map<Symbol, SymbolBinding>> functions = new Stack<>();
  private final Stack<Map<Symbol, SymbolBinding>> variables = new Stack<>();
  private final List<SymbolBinding> retired = new ArrayList<>();
  private final SyntaxAnalyzer analyzer;

  public LexicalBindingManager(SyntaxAnalyzer analyzer) {
    this.analyzer = analyzer;
  }

  public void dropLexicalVariables() {
    retired.addAll(variables.pop().values());
  }

  public void dropLexicalFunctions() {
    retired.addAll(functions.pop().values());
  }

  public void registerFunctionUsage(LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolBinding symbolBinding = getFunctionBinding(symbolName);
    symbolBinding.addUsage(symbol);
  }

  public void registerVariableUsage(LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolBinding symbolBinding = getVariableBinding(symbolName);
    symbolBinding.addUsage(symbol);
  }

  public LexicalScope defineLexicalVariables(LispList container, List<LispSymbol> variableList) {
    Map<Symbol, SymbolBinding> newDictionary = new HashMap<>();
    for (LispSymbol symbolExpression : variableList) {
      Symbol symbol = analyzer.packageManager.getSymbol(symbolExpression.getText());
      SymbolBinding symbolBinding = new SymbolBinding(symbol, VARIABLE, LEXICAL);
      symbolBinding.setDefinition(container, symbolExpression);
      newDictionary.put(symbol, symbolBinding);
    }
    variables.push(newDictionary);
    return this::dropLexicalVariables;
  }

  public LexicalScope defineLexicalFunctions(LispList container, List<LispSymbol> functionList) {
    Map<Symbol, SymbolBinding> newDictionary = new HashMap<>();
    for (LispSymbol symbolExpression : functionList) {
      Symbol symbol = analyzer.packageManager.getSymbol(symbolExpression.getText());
      SymbolBinding symbolBinding = new SymbolBinding(symbol, FUNCTION, LEXICAL);
      symbolBinding.setDefinition(container, symbolExpression);
      newDictionary.put(symbol, symbolBinding);
    }
    functions.push(newDictionary);
    return this::dropLexicalFunctions;
  }

  public SymbolBinding getVariableBinding(String symbolName) {
    Symbol symbol = analyzer.packageManager.getSymbol(symbolName);
    for (int i = variables.size() - 1; i >= 0; i--) {
      SymbolBinding binding = variables.get(i).get(symbol);
      if (binding != null) return binding;
    }
    return analyzer.packageManager.getVariable(symbol);
  }

  private SymbolBinding getFunctionBinding(String symbolName) {
    Symbol symbol = analyzer.packageManager.getSymbol(symbolName);
    for (int i = functions.size() - 1; i >= 0; i--) {
      SymbolBinding binding = functions.get(i).get(symbol);
      if (binding != null) return binding;
    }
    return analyzer.packageManager.getFunction(symbol);
  }

  public boolean isEmpty() {
    return functions.empty() && variables.empty();
  }

  public Collection<SymbolBinding> getRetired() {
    return retired;
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
