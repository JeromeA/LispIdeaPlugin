package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

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
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    LispSexp functionName = form.getSexpList().get(0);
    if (functionName.getSymbol() == null) {
      context.highlighter.highlightError(functionName, "Function name expected");
      return;
    }
    Symbol symbol = context.getSymbol(functionName.getSymbol());
    if (KEYWORDS.contains(symbol)) {
      context.highlighter.highlightKeyword(functionName);
    }
    context.addFunctionUsage(functionName.getSymbol());
    context.analyzer.analyzeForms(form.getSexpList(), 1);
  }
}
