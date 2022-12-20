package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.psi.LispPackagePrefix;
import org.ax1.lisp.psi.LispSymbol;

public class AnalysisContext {
  public final Highlighter highlighter;
  public final PackageManager packageManager;
  public final SyntaxAnalyzer analyzer;
  public final Bindings result;
  public final LexicalBindingManager lexicalBindings;
  private String currentPackage;

  public AnalysisContext(Highlighter highlighter, PackageManager packageManager, SyntaxAnalyzer analyzer) {
    this.highlighter = highlighter;
    this.packageManager = packageManager;
    this.analyzer = analyzer;
    this.result = new Bindings();
    this.lexicalBindings = new LexicalBindingManager();
    currentPackage = CommonLispUserPackage.COMMON_LISP_USER;
  }

  public void addFunctionDefinition(LispSymbol symbol, String description) {
    result.addFunctionDefinition(getSymbol(symbol), symbol.getSymbolName(), description);
  }

  public void addMethodDefinition(LispSymbol symbol, String description) {
    result.addMethodDefinition(getSymbol(symbol), symbol.getSymbolName(), description);
  }

  public void addVariableDefinition(LispSymbol symbol, String description) {
    result.addVariableDefinition(getSymbol(symbol), symbol.getSymbolName(), description);
  }

  public void addFunctionUsage(LispSymbol fullSymbolName) {
    Symbol symbol = getSymbol(fullSymbolName);
    SymbolDefinition lexicalFunction = lexicalBindings.getLexicalFunction(symbol);
    if (lexicalFunction != null) {
      lexicalFunction.getUsages().add(fullSymbolName.getSymbolName());
      return;
    }
    result.addFunctionUsage(symbol, fullSymbolName.getSymbolName());
  }

  public void addVariableUsage(LispSymbol fullSymbolName) {
    Symbol symbol = getSymbol(fullSymbolName);
    SymbolDefinition lexicalVariable = lexicalBindings.getLexicalVariable(symbol);
    if (lexicalVariable != null) {
      lexicalVariable.getUsages().add(fullSymbolName.getSymbolName());
      return;
    }
    result.addVariableUsage(symbol, fullSymbolName.getSymbolName());
  }

  public Symbol getSymbol(LispSymbol sexp) {
    return getSymbol(sexp, null);
  }

  public Symbol getSymbol(LispSymbol fullSymbol, String nameOverride) {
    String symbolName = nameOverride != null ? nameOverride : fullSymbol.getSymbolName().getValue();
    if (fullSymbol.getColon() != null) {
      LispPackagePrefix packagePrefix = fullSymbol.getPackagePrefix();
      if (packagePrefix == null) {
        return KeywordPackage.INSTANCE.intern(symbolName);
      }
      String packageName = packagePrefix.getValue();
      if (packageName.equals("#")) {
        return new Symbol("", symbolName);
      }
      result.addPackageUsage(packagePrefix);
      LispPackage lispPackage = packageManager.getOrCreatePackage(packageName);
      return lispPackage.intern(symbolName);
    }
    return packageManager.getOrCreatePackage(currentPackage).intern(symbolName);
  }

  public LocatedSymbol getLocatedSymbol(LispSymbol parsedSymbol) {
    return new LocatedSymbol(getSymbol(parsedSymbol), parsedSymbol.getSymbolName());
  }

  public void setCurrentPackage(String name) {
    currentPackage = name;
  }
}
