package org.ax1.lisp.analysis;

import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.*;

import static org.ax1.lisp.analysis.SymbolDescriptor.BindingType.DYNAMIC;
import static org.ax1.lisp.analysis.SymbolDescriptor.BindingType.LEXICAL;

class SymbolStack {
  private final Map<String, SymbolDescriptor> special = new HashMap<>();
  private final Stack<Map<String, SymbolDescriptor>> lexical = new Stack<>();
  private final List<SymbolDescriptor> retired = new ArrayList<>();
  private final SymbolDescriptor.SymbolType symbolType;

  public SymbolStack(SymbolDescriptor.SymbolType symbolType) {
    this.symbolType = symbolType;
  }

  public void registerUsage(LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolDescriptor symbolDescriptor = getSymbol(symbolName);
    symbolDescriptor.addUsage(symbol);
    symbol.setSymbolCache(symbolDescriptor);
  }

  public void registerSpecialDefinition(LispList container, LispSymbol symbol) {
    String symbolName = symbol.getText();
    SymbolDescriptor symbolDescriptor = special.get(symbolName);
    if (symbolDescriptor == null) {
      symbolDescriptor = new SymbolDescriptor(symbolName, symbolType, DYNAMIC);
      special.put(symbolName, symbolDescriptor);
    }
    symbolDescriptor.setDefinition(container, symbol);
    symbol.setSymbolCache(symbolDescriptor);
  }

  public void registerLexicalDefinitions(LispList container, List<LispSymbol> variableList) {
    Map<String, SymbolDescriptor> newDictionary = new HashMap<>();
    for (LispSymbol symbol : variableList) {
      String symbolName = symbol.getText();
      SymbolDescriptor symbolDescriptor = new SymbolDescriptor(symbolName, symbolType, LEXICAL);
      symbolDescriptor.setDefinition(container, symbol);
      newDictionary.put(symbolName, symbolDescriptor);
      symbol.setSymbolCache(symbolDescriptor);
    }
    lexical.push(newDictionary);
  }

  public void dropLexicalDefinitions() {
    retired.addAll(lexical.pop().values());
  }

  private SymbolDescriptor getSymbol(String symbolName) {
    for (int i = lexical.size() - 1; i >= 0; i--) {
      SymbolDescriptor symbol = lexical.get(i).get(symbolName);
      if (symbol != null) return symbol;
    }
    SymbolDescriptor symbol = special.get(symbolName);
    if (symbol == null) {
      symbol = new SymbolDescriptor(symbolName, symbolType, DYNAMIC);
      special.put(symbolName, symbol);
    }
    return symbol;
  }

  public Stack<Map<String, SymbolDescriptor>> getLexical() {
    return lexical;
  }

  public Iterable<SymbolDescriptor> getRetired() {
    return retired;
  }

  public Map<String, SymbolDescriptor> getSpecial() {
    return special;
  }
}
