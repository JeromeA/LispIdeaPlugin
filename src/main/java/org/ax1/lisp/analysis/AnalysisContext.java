package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.PackageManager;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.analysis.symbol.SymbolDefinition;
import org.ax1.lisp.psi.LispSexp;

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

  public void addFunctionDefinition(LispSexp symbolName, String description) {
    result.addFunctionDefinition(packageManager.getSymbol(symbolName), symbolName, description);
  }

  public void addMethodDefinition(LispSexp symbolName, String description) {
    result.addMethodDefinition(packageManager.getSymbol(symbolName), symbolName, description);
  }

  public void addVariableDefinition(LispSexp symbolName, String description) {
    result.addVariableDefinition(packageManager.getSymbol(symbolName), symbolName, description);
  }

  public void addFunctionUsage(LispSexp symbolName) {
    Symbol symbol = packageManager.getSymbol(symbolName);
    SymbolDefinition lexicalFunction = lexicalBindings.getLexicalFunction(symbol);
    if (lexicalFunction != null) {
      lexicalFunction.getUsages().add(symbolName);
      return;
    }
    result.addFunctionUsage(symbol, symbolName);
  }

  public void addVariableUsage(LispSexp symbolName) {
    Symbol symbol = packageManager.getSymbol(symbolName);
    SymbolDefinition lexicalVariable = lexicalBindings.getLexicalVariable(symbol);
    if (lexicalVariable != null) {
      lexicalVariable.getUsages().add(symbolName);
      return;
    }
    result.addVariableUsage(symbol, symbolName);
  }
}
