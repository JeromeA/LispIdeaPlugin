package org.ax1.lisp.analysis.form;

import org.ax1.lisp.analysis.AnalysisContext;
import org.ax1.lisp.psi.LispList;
import org.ax1.lisp.psi.LispSexp;

import java.util.List;

public class AnalyzeCase implements FormAnalyzer {

  @Override
  public void analyze(AnalysisContext context, LispList form) {
    List<LispSexp> list = form.getSexpList();
    if (list.size() < 2) {
      context.highlighter.highlightError(form, "CASE needs at least 1 argument");
      return;
    }
    context.analyzer.analyzeForm(list.get(1));
    for (int i = 2; i < list.size(); i++) {
      LispList clause = list.get(i).getList();
      if (clause == null || clause.getSexpList().isEmpty()) {
        context.highlighter.highlightError(list.get(i), "normal clause expected");
        continue;
      }
      context.highlighter.highlightConstant(clause.getSexpList().get(0));
      context.analyzer.analyzeForms(clause.getSexpList(), 1);
    }
  }
}
