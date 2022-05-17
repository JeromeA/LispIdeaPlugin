package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.Set;

public class AnalyzeFunctionCall implements FormAnalyzer {

  /**
   * Names that could look like function calls, but behave effectively like language keywords and should
   * be highlighted as such.
   */
  private static final Set<String> KEYWORDS =
      Set.of("DECLARE", "IF", "IGNORE", "RETURN", "SETQ", "SPECIAL", "UNLESS", "WHEN");

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    LispSymbol symbolName = form.getSexpList().get(0).getSymbol();
    if (symbolName == null) {
      analyzer.annotations.highlightError(form.getSexpList().get(0), "Function name expected");
      return;
    }
    Symbol symbol = analyzer.packageManager.getSymbol(symbolName.getText());
    if (KEYWORDS.contains(symbol.getName())) {
      analyzer.annotations.highlightKeyword(symbolName);
    }
    analyzer.lexicalBindings.registerFunctionUsage(symbolName);
    analyzer.analyzeForms(form.getSexpList(), 1);
  }
}