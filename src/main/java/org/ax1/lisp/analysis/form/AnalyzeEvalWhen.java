package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.analysis.symbol.Symbol;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;
import java.util.Set;

import static org.ax1.lisp.analysis.symbol.Symbol.keywordSymbol;

public class AnalyzeEvalWhen implements FormAnalyzer {

  private static final Set<Symbol> SITUATIONS = Set.of(
      keywordSymbol("COMPILE-TOPLEVEL"),
      keywordSymbol("LOAD-TOPLEVEL"),
      keywordSymbol("EXECUTE")
  );

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "EVAL-WHEN needs at least 1 argument");
      return;
    }
    LispList situationList = list.get(1).getList();
    if (situationList == null) {
      context.highlighter.highlightError(list.get(1), "List of situations expected");
      return;
    }
    for (LispSexp situation : situationList.getSexpList()) {
      LispSymbol symbol = situation.getSymbol();
      if (symbol != null && SITUATIONS.contains(context.packageManager.getSymbol(situation))) {
        context.highlighter.highlightKeyword(situation);
      } else {
        context.highlighter.highlightError(situation, "Invalid situation");
      }
    }

    context.analyzer.analyzeForms(list, 2);
  }
}
