package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.SyntaxAnalyzer;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSymbol;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AnalyzeFunctionCall implements FormAnalyzer {

  /**
   * Names that could look like function calls, but behave effectively like language keywords and should
   * be highlighted as such.
   */
  private static final Set<Symbol> KEYWORDS =
      Stream.of("DECLARE", "IF", "IGNORE", "RETURN", "SETQ", "SPECIAL", "UNLESS", "WHEN")
          .map(Symbol::clSymbol)
          .collect(Collectors.toSet());

  @Override
  public void analyze(SyntaxAnalyzer analyzer, LispList form) {
    analyzer.annotations.highlightKeyword(form);
    LispSymbol parsedSymbol = form.getSexpList().get(0).getSymbol();
    if (parsedSymbol == null) {
      analyzer.annotations.highlightError(form.getSexpList().get(0), "Function name expected");
      return;
    }
    Symbol symbol = analyzer.packageManager.getSymbol(parsedSymbol);
    if (KEYWORDS.contains(symbol)) {
      analyzer.annotations.highlightKeyword(parsedSymbol);
    }
    analyzer.lexicalBindings.registerFunctionUsage(symbol, parsedSymbol);
    analyzer.analyzeForms(form.getSexpList(), 1);
  }
}
