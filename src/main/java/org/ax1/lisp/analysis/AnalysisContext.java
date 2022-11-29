package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispSymbol;

public class AnalysisContext {
  public final Highlighter highlighter;
  public final PackageManager packageManager;
  public final SyntaxAnalyzer analyzer;
  public final Bindings result;
  public final LexicalBindingManager lexicalBindings;

  public AnalysisContext(Highlighter highlighter, PackageManager packageManager, SyntaxAnalyzer analyzer) {
    this.highlighter = highlighter;
    this.packageManager = packageManager;
    this.analyzer = analyzer;
    this.result = new Bindings();
    this.lexicalBindings = new LexicalBindingManager();
  }

  public void addFunctionDefinition(LispSymbol symbolName) {
    result.addFunctionDefinition(packageManager.getSymbol(symbolName), symbolName);
  }

  public void addMethodDefinition(LispSymbol symbolName) {
    result.addMethodDefinition(packageManager.getSymbol(symbolName), symbolName);
  }

  public void addVariableDefinition(LispSymbol symbolName) {
    result.addVariableDefinition(packageManager.getSymbol(symbolName), symbolName);
  }

  public void addFunctionUsage(LispSymbol symbolName) {
    Symbol symbol = packageManager.getSymbol(symbolName);
    SymbolBinding lexicalFunction = lexicalBindings.getLexicalFunction(symbol);
    if (lexicalFunction != null) {
      lexicalFunction.usages.add(symbolName);
      return;
    }
    result.addFunctionUsage(symbol, symbolName);
  }

  public void addVariableUsage(LispSymbol symbolName) {
    Symbol symbol = packageManager.getSymbol(symbolName);
    SymbolBinding lexicalVariable = lexicalBindings.getLexicalVariable(symbol);
    if (lexicalVariable != null) {
      lexicalVariable.usages.add(symbolName);
      return;
    }
    result.addVariableUsage(symbol, symbolName);
  }
}
