package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

import static org.ax1.lisp.analysis.SymbolBinding.BindingType.LEXICAL;
import static org.ax1.lisp.analysis.SymbolBinding.SymbolType.VARIABLE;

public class LexicalSymbolManager {

  private final LexicalDrop lexicalVariableDrop = () -> dropLexicalVariables();
  private final LexicalDrop lexicalFunctionDrop = () -> dropLexicalFunctions();

  private final Stack<Map<String, SymbolBinding>> functions = new Stack<>();
  private final Stack<Map<String, SymbolBinding>> variables = new Stack<>();
  private final List<SymbolBinding> retired = new ArrayList<>();
  private final DynamicSymbolManager dynamicSymbolManager;

  public LexicalSymbolManager(DynamicSymbolManager dynamicSymbolManager) {
    this.dynamicSymbolManager = dynamicSymbolManager;
  }

  public void dropLexicalVariables() {
    retired.addAll(variables.pop().values());
  }

  public void dropLexicalFunctions() {
    retired.addAll(functions.pop().values());
  }

  public void registerFunctionUsage(LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolBinding symbolBinding = getFunctionSymbol(symbolName);
    symbolBinding.addUsage(symbol);
    symbol.setSymbolBinding(symbolBinding);
  }

  public void registerVariableUsage(LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolBinding symbolBinding = getVariableSymbol(symbolName);
    symbolBinding.addUsage(symbol);
    symbol.setSymbolBinding(symbolBinding);
  }

  public LexicalDrop defineLexicalVariables(LispList container, List<LispSymbol> variableList) {
    Map<String, SymbolBinding> newDictionary = new HashMap<>();
    for (LispSymbol symbol : variableList) {
      String symbolName = symbol.getText();
      SymbolBinding symbolBinding = new SymbolBinding(symbolName, VARIABLE, LEXICAL);
      symbolBinding.setDefinition(container, symbol);
      newDictionary.put(symbolName, symbolBinding);
      symbol.setSymbolBinding(symbolBinding);
    }
    variables.push(newDictionary);
    return lexicalVariableDrop;
  }

  private SymbolBinding getVariableSymbol(String symbolName) {
    for (int i = variables.size() - 1; i >= 0; i--) {
      SymbolBinding symbol = variables.get(i).get(symbolName);
      if (symbol != null) return symbol;
    }
    return dynamicSymbolManager.getSymbolDescriptor(symbolName).getVariable();
  }

  private SymbolBinding getFunctionSymbol(String symbolName) {
    for (int i = functions.size() - 1; i >= 0; i--) {
      SymbolBinding symbol = functions.get(i).get(symbolName);
      if (symbol != null) return symbol;
    }
    return dynamicSymbolManager.getSymbolDescriptor(symbolName).getFunction();
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
