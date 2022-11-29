package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;
import org.ax1.lisp.psi.LispSymbol;

import java.util.List;

public class AnalyzeDefparameter implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    context.highlighter.highlightKeyword(form);
    List<LispSexp> sexpList = form.getSexpList();
    if (sexpList.size() < 3) {
      context.highlighter.highlightError(form, "DEFPARAMETER takes at least 2 arguments");
      return;
    }
    LispSymbol symbol = sexpList.get(1).getSymbol();
    if (symbol == null) {
      context.highlighter.highlightError(sexpList.get(1), "Variable name expected");
      return;
    }
    context.addVariableDefinition(symbol);
    context.analyzer.analyzeForm(sexpList.get(2));
  }
}
