package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolBinding;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

import static org.ax1.lisp.analysis.symbol.SymbolBinding.BindingType.LEXICAL;
import static org.ax1.lisp.analysis.symbol.SymbolBinding.SymbolType.VARIABLE;

public class LexicalBindingManager {

  private final LexicalDrop lexicalVariableDrop = () -> dropLexicalVariables();
  private final LexicalDrop lexicalFunctionDrop = () -> dropLexicalFunctions();

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
    symbol.setSymbolBinding(symbolBinding);
  }

  public void registerVariableUsage(LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolBinding symbolBinding = getVariableBinding(symbolName);
    symbolBinding.addUsage(symbol);
    symbol.setSymbolBinding(symbolBinding);
  }

  public LexicalDrop defineLexicalVariables(LispList container, List<LispSymbol> variableList) {
    Map<Symbol, SymbolBinding> newDictionary = new HashMap<>();
    for (LispSymbol symbol : variableList) {
      String symbolName = symbol.getText();
      SymbolBinding symbolBinding = new SymbolBinding(symbolName, VARIABLE, LEXICAL);
      symbolBinding.setDefinition(container, symbol);
      newDictionary.put(analyzer.symbolManager.getSymbol(symbolName), symbolBinding);
      symbol.setSymbolBinding(symbolBinding);
    }
    variables.push(newDictionary);
    return lexicalVariableDrop;
  }

  private SymbolBinding getVariableBinding(String symbolName) {
    Symbol symbol = analyzer.symbolManager.getSymbol(symbolName);
    for (int i = variables.size() - 1; i >= 0; i--) {
      SymbolBinding binding = variables.get(i).get(symbol);
      if (binding != null) return binding;
    }
    return analyzer.symbolManager.getVariable(symbol);
  }

  private SymbolBinding getFunctionBinding(String symbolName) {
    Symbol symbol = analyzer.symbolManager.getSymbol(symbolName);
    for (int i = functions.size() - 1; i >= 0; i--) {
      SymbolBinding binding = functions.get(i).get(symbol);
      if (binding != null) return binding;
    }
    return analyzer.symbolManager.getFunction(symbol);
  }

  public boolean isEmpty() {
    return functions.empty() && variables.empty();
  }

  public Iterable<SymbolBinding> getRetired() {
    return retired;
  }

  public interface LexicalDrop extends AutoCloseable {
    @Override
    public void close();
  }
}
