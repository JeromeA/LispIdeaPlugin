package org.ax1.lisp.analysis;

import org.ax1.lisp.analysis.symbol.*;
import org.ax1.lisp.psi.LispSexp;

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

  public void addFunctionDefinition(LispSexp symbolName, String description) {
    result.addFunctionDefinition(getSymbol(symbolName), symbolName, description);
  }

  public void addMethodDefinition(LispSexp symbolName, String description) {
    result.addMethodDefinition(getSymbol(symbolName), symbolName, description);
  }

  public void addVariableDefinition(LispSexp symbolName, String description) {
    result.addVariableDefinition(getSymbol(symbolName), symbolName, description);
  }

  public void addFunctionUsage(LispSexp symbolName) {
    Symbol symbol = getSymbol(symbolName);
    SymbolDefinition lexicalFunction = lexicalBindings.getLexicalFunction(symbol);
    if (lexicalFunction != null) {
      lexicalFunction.getUsages().add(symbolName);
      return;
    }
    result.addFunctionUsage(symbol, symbolName);
  }

  public void addVariableUsage(LispSexp symbolName) {
    Symbol symbol = getSymbol(symbolName);
    SymbolDefinition lexicalVariable = lexicalBindings.getLexicalVariable(symbol);
    if (lexicalVariable != null) {
      lexicalVariable.getUsages().add(symbolName);
      return;
    }
    result.addVariableUsage(symbol, symbolName);
  }

  public Symbol getSymbol(LispSexp sexp) {
    return getSymbol(sexp, sexp.getText());
  }

  public Symbol getSymbol(LispSexp sexp, String name) {
    name = name.toUpperCase();
    if (name.startsWith("#:")) {
      return new Symbol("", name.substring(2));
    }
    if (name.startsWith(":")) {
      return KeywordPackage.INSTANCE.intern(name.substring(1));
    }
    int doubleColon = name.indexOf("::");
    String packageName = null;
    String symbolName = null;
    if (doubleColon > 0) {
      packageName = name.substring(0, doubleColon);
      symbolName = name.substring(doubleColon + 2);
    }
    int colon = name.indexOf(":");
    if (colon > 0 && doubleColon < 0) {
      packageName = name.substring(0, colon);
      symbolName = name.substring(colon + 1);
    }
    if (colon > 0) {
      LispPackage lispPackage = packageManager.getOrCreatePackage(packageName);
      result.addPackageUsage(lispPackage.getName(), sexp);
      // If we were Lisp, we would call findSymbol or findExportedSymbol, which can return null. But we want to be able
      // to manipulate that unknown symbol, find all its occurrences, etc, so we really want it to exist. If we need to
      // mark it as invalid, this will have to be done at a later stage.
      return lispPackage.intern(symbolName);
    }
    return packageManager.getOrCreatePackage(currentPackage).intern(name);
  }

  public LocatedSymbol getLocatedSymbol(LispSexp parsedSymbol) {
    return new LocatedSymbol(getSymbol(parsedSymbol), parsedSymbol);
  }

  public void setCurrentPackage(String name) {
    currentPackage = name;
  }
}
